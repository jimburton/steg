steg
====

A simple [steganography](http://en.wikipedia.org/wiki/Steganography) tool for teaching purposes in the Haskell course at the University of Brighton. It is used to demonstrate approaches to parsing and refactoring techniques, through refinements to an initial solution.

The idea is to hide a text message within a binary file. This works well with image formats, because small changes to binary content don't corrupt the file and result in tiny changes to colour values of pixels, too small to be detected by the human eye. They can, or course, be detected by an image diff tool like ImageMagick's `compare` utility, but only if you have access to the original.

Using the tool
==============

````
steg$ cabal install
steg$ cat message.txt
Hi, how are you?
steg$ steg bury image.pgm message.txt image2.pgm
steg$ steg dig image2.pgm
Hi, how are you?
````
How it works
============

First, encode an ASCII message as a binary sequence. For instance, in the string "ab", 'a' has the ASCII value 97, while 'b' is 98. The whole string is encoded as `0110000101100010`, which is the concatenation of the binary numbers equal to 97 and 98. 

Then, take the *body* of a binary image file. The body is that part of the content which comes after a *header*, which contains metadata about the file, such as its format type, width and height, etc. The body contains the actual picture. Modify the data to overwrite the *least-significant bit* (LSB) of each byte so that it contains part of the binary-encoded message. Say we want to store the message "a", `01100001` in binary, and our image file uses a simple greyscale format in which each byte in the data represents a single pixel. The first eight bytes of the data might look as follows:
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
We need to change the last bit of every byte to the corresponding bit from the message, `01100001`:
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
Because this results in the change of a pixel's greyscale value from, say, 10 to 11, there is no human-visible effect. To extract the message, we collect the LSBs of modified data:
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

Note that this would work just as well for RGB formats that represent each pixel by three bytes, but we would be changing fewer pixels. To make it easier to recover the message, we will want to know its length. So, the LSBs in the first few bytes will contain a number. If we are happy with 255 as the length of the longest message we can hide, the number can be stored in one byte, hidden in the LSBs of the first 8 bytes.   

Ways to improve the program
---------------------------

* Add support for more image formats.
* Change the CLI to use `System.GetOpt` and be more flexible by adding options to read in the message from `stdin`, output the result to `stdout` and so on. 
* If you suspected that this technique might have been used to hide something in an image file, it would be easy enough to collect the LSBs and check whether they do represent a value in some character encoding. Increase the obfuscation by making the first thing that somes after the message length be an *offset* and *step*, then start reading
    at *offset* and jump forward *step* bytes to read the next LSB.