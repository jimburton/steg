-- |
-- | Module      :  Parse.hs
-- | Description :  Parsing and modifying binary files
-- | Copyright   :  (c) Jim Burton
-- | License     :  MIT
-- |
-- | Maintainer  :  j.burton@brighton.ac.uk
-- | Stability   :  provisional
-- | Portability :  portable
-- |
module Steg.Parse (dig, bury, buryByteString) where

import qualified Data.Binary.Strict.BitGet as BG
import Data.Bits (clearBit, setBit)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Char (chr)
import Data.List (elemIndices)
import Data.Word8 (Word8)
import Steg.Format.BMP ( parseBMP )
import Steg.Format.PGM ( parsePGM )
import Steg.Format.StegFormat
  ( Format (..),
    Steg (..),
    StegBox (..),
    magicNumbers,
  )

-- | Parse a ByteString
bsToSteg :: B.ByteString -> Maybe StegBox
bsToSteg bs =
  case idHeader bs of
    Just PGM -> Steg.Format.PGM.parsePGM bs
    Just BMP -> parseBMP bs
    Nothing -> Nothing

-- | Identify the format of some data from its magic number.
idHeader :: B.ByteString -> Maybe Format
idHeader bs =
  let mn = L8.unpack $ L8.fromChunks [B.take 2 bs]
   in lookup mn magicNumbers

-- | Bury some text in the data read in from the first file, writing
-- | the result out to the second file.
bury :: FilePath -> FilePath -> FilePath -> IO ()
bury inPath txtPath outPath = B.readFile txtPath >>= buryByteString inPath outPath

-- | A function like bury but which takes the message as a ByteString.
buryByteString :: FilePath -> FilePath -> B.ByteString -> IO ()
buryByteString inPath outPath bs = do
  mg <- bsToSteg <$> B.readFile inPath
  case mg of
    Nothing -> do
      putStrLn "bsToSteg: no output"
      return ()
    Just (StegBox g) ->
      do
        let bs' = L8.toStrict $ L8.filter (/= '\n') (L8.fromChunks [bs])
            lenWord = fromIntegral (B.length bs') :: Word8
            lenWBits = bsToBits (B.cons lenWord B.empty)
            bits = lenWBits ++ bsToBits bs'
            g' = setData g (modifyLSBs (getData g) bits)
        output outPath $ StegBox g'

-- | Write some data out to a file.
output :: FilePath -> StegBox -> IO ()
output path (StegBox s) = B.writeFile path $ sGetContents s

-- | Read in a file and try to construct a message from the contents of the LSBs.
dig :: FilePath -> IO (Maybe String)
dig binPath = do
  mg <- bsToSteg <$> B.readFile binPath
  case mg of
    Nothing -> return Nothing
    Just (StegBox g) -> do
      let lsbs = getLSBs $ getData g
          bitLen = 8 * binToDec (take 8 lsbs)
          result =
            filter (\c -> c /= '\'' && c /= '\n') $
              boolsToStr (take bitLen (drop 8 lsbs))
      return $ Just result

-- | Transform a list of Bools (Bits) into a String.
boolsToStr :: [Bool] -> String
boolsToStr bs =
  if length bs < 8
    then ""
    else show (chr $ binToDec (take 8 bs)) ++ boolsToStr (drop 8 bs)

getLSBs :: B.ByteString -> [Bool]
getLSBs = B.foldr (\x acc -> either error (: acc) (getLSB (B.pack [x]))) []

-- | Get the LSB from a ByteString, which should contain a single word.
getLSB :: B.ByteString -> Either String Bool
getLSB bs = BG.runBitGet bs $ do
  BG.skip 7
  BG.getBit

-- | Set the LSB in an 8-bit Word.
setLSB :: Bool -> Word8 -> Word8
setLSB b w = if b then setBit w 0 else clearBit w 0

-- | Transform an 8-bit Word into a list of Bools.
wordToBits :: Integral a => a -> [a]
wordToBits = pad . reverse . decToBin
  where
    pad xs = replicate (8 - length xs) 0 ++ xs
    decToBin 0 = []
    decToBin y = let (a, b) = quotRem y 2 in b : decToBin a

-- | Transform a ByteString into a list of Words.
bsToBits :: B.ByteString -> [Word8]
bsToBits = B.foldr ((++) . wordToBits) []

-- | Modify the LSB in each byte of the first argument.
modifyLSBs :: B.ByteString -> [Word8] -> B.ByteString
modifyLSBs bs [] = bs
modifyLSBs bs (w : ws) =
  case B.uncons bs of
    Nothing -> B.empty
    (Just (w', bs')) -> B.cons (setLSB (w == 1) w') (modifyLSBs bs' ws)

-- | Convert binary to decimal.
binToDec :: [Bool] -> Int
binToDec = sum . map (2 ^) . elemIndices True . reverse
