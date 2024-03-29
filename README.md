steg
====

A simple [steganography](http://en.wikipedia.org/wiki/Steganography)
tool for teaching purposes in the Haskell course at the University of
Brighton. It is used to demonstrate approaches to parsing and
refactoring techniques, through refinements to an initial solution.

The idea is to hide a text message within a binary file. This works
well with image formats, because small changes to binary content don't
corrupt the file and result in tiny changes to colour values of
pixels, too small to be detected by the human eye. They can, or
course, be detected by an image diff tool like ImageMagick's `compare`
utility, but only if you have access to the original. steg works for
RAW PGM (greyscale) files and Windows Bitmap (BMP) files so far.

Using the tool
==============

````
$ cabal update
$ cabal configure --enable-tests
$ cabal install
$ cat etc/samples/soseki.txt
Sosuke had been relaxing for some time on the veranda...
$ cabal run steg -- bury etc/samples/bmp/24bit/duck.bmp etc/samples/soseki.txt ~/tmp/image.bmp 
$ cabal run steg -- dig ~/tmp/image.bmp
Sosuke had been relaxing for some time on the veranda...
````

You shouldn't be able to see any difference if you inspect the two images 
yourself. However, if you have `imagemagick` installed you can generate an
image that highlights the differences in each corresponding pair of pixels:

```
$ compare etc/samples/24bit/duck.bmp ~/tmp/image.bmp ~/tmp/difference.bmp
```

Note that there are sample image files in the directory `etc/samples`. 

How it works
============

First, encode the ASCII message to be hidden as a binary sequence. For
instance, in the string "ab", 'a' has the ASCII value 97, while 'b'
is 98. The whole string is encoded as `0110000101100010`, which is the
concatenation of the binary numbers equal to 97 and 98.

Then, take the *body* of a binary image file. The body is that part of
the content which comes after the *header*, which contains metadata
about the file, such as its format type, width and height, etc. The
body contains the data making up the actual picture. 

Modify the data to overwrite the *least-significant bit* (LSB) of each
byte so that it contains part of the binary-encoded message. Say we
want to store the message "a", `01100001` in binary, and our image
file uses a simple greyscale format in which each byte in the data
represents a single pixel. The first eight bytes of the data might
look as follows:

````
00010101
10010001 
01011111 
10111100 
10010001 
00010101 
11011100 
10010001 
```` 

We need to change the last bit of every byte to the corresponding bit
from the message, `01100001`: 

```` 
00010101 ---> 00010100 (flip)
10010001 ---> 10010001 (no change) 
01011111 ---> 01011111 (no change)
10111100 ---> 10111100 (no change) 
10010001 ---> 10010000 (flip)
00010101 ---> 00010100 (flip) 
11011100 ---> 11011100 (no change)
10010000 ---> 10010001 (flip) 
```` 

Because this results in the change of a pixel's greyscale value from,
say, 53 to 52, there is no human-visible effect. To extract the
message, we collect the LSBs of modified data: 

```` 
0001010[0]
1001000[1] 
0101111[1] 
1011110[0] 
1001000[0] 
0001010[0] 
1101110[0]
1001000[1] 
```` 

Giving us back the message `01100001`.

Note that this would work just as well for RGB formats that represent
each pixel by three bytes, but we would be changing fewer pixels. To
make it easier to recover the message, we will want to know its
length. So, the LSBs in the first few bytes will contain a number. If
we are happy with 255 as the length of the longest message we can
hide, the number can be stored in one byte, hidden in the LSBs of the
first 8 bytes.

Ways to improve the program 
---------------------------

* Add support for more image formats. There are several codecs
  [available on hackage](https://hackage.haskell.org/packages/#cat:Codec),
  or follow the approach in `Steg.PGM` to write your own.
* Add support for different character encodings. The code currently assumes
  that a character is stored in a single byte, which won't work for
  encodings like `utf16`. Detect the character encoding when reading a file 
  and read the right number of bits for each character. 
* Change the CLI to use `System.GetOpt` and be more flexible by adding
  options to read in the message from `stdin`, output the result to
  `stdout` and so on.
* If someone suspected that this technique had been used to hide
  something in an image file, it would be easy enough to collect the
  LSBs and check whether they do represent a value in some character
  encoding. You can make this task much harder by encrypting the
  message before hiding it, and distributing parts of the message
  randomly within the data. For the the encryption, use a simple
  one-time pad algorithm such as a
  [Vernam cipher](http://mess.ninjalith.com/cs/stream_ciphers). Hide
  the *key* directly after the message length, to be used when
  recovering the message. To incorporate randomness, create a random
  number generator (RNG) based on some *seed*. Then hide each bit of
  the message in random locations generated by your RNG. Of course,
  you need to make sure not to overwrite any LSBs that have been
  written to before. Then encode the seed at the beginning of the
  data, and use it as the basis of a PRNG when recovering the
  message. Now, in order to recover your message, an attacker would
  have to work out what you had done *and* use the seed to create a
  PRNG that uses the same algorithm as yours.
