module Steg.Parse
    (dig, bury)
    where

import           Control.Applicative ((<$>))
import qualified Data.Binary.Strict.BitGet as BG
import           Data.Bits (setBit, clearBit)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Char (chr)
import           Data.Int (Int64)
import           Data.List (elemIndices)
import           Data.Word (Word8)
import           Data.Word8 (_cr)
import           Steg.Format.BMP
import           Steg.Format.PGM
import           Steg.Format.StegFormat (Steg(..), StegBox(..))
import           Steg.Info (Format(..), signature)

countWords :: Steg t => t -> Int64
countWords = L.length . getData 

setLSB :: Bool -> Word8 -> Word8
setLSB b w = if b then setBit w 0 else clearBit w 0

wordToBits :: Integral a => a -> [a]
wordToBits = pad . reverse . decToBin 
    where pad xs     = replicate (8 - length xs) 0 ++ xs
          decToBin 0 = []
          decToBin y = let (a,b) = quotRem y 2 in b : decToBin a

bsToBits :: L.ByteString -> [Word8]
bsToBits = bsToBits' []
    where bsToBits' xs bs =  
            let h = L.uncons bs in
            case h of
              Nothing -> xs
              Just (w, bs') -> bsToBits' (wordToBits w ++ xs) bs'

modifyLSBs :: L.ByteString -> [Word8] -> L.ByteString
modifyLSBs bs []     = bs
modifyLSBs bs (w:ws) = 
    let mw = L.uncons bs in
    case mw of
      Nothing -> L.empty
      (Just (w', bs')) -> L.cons (setLSB (w==1) w') (modifyLSBs bs' ws) 

bsToSteg :: L.ByteString -> Maybe (StegBox, L8.ByteString)
bsToSteg bs = 
    case idHeader bs of
      PGM -> parsePGM bs
      BMP -> parseBMP bs
                      
idHeader :: L.ByteString -> Format
idHeader _ = PGM

bury :: FilePath -> FilePath -> FilePath -> IO ()
bury binPath txtPath outPath = do
  mg <- bsToSteg <$> L.readFile binPath
  case mg of
    Nothing -> return ()
    Just (StegBox g, _) -> do
      s <- L8.readFile txtPath
      if L8.length s > 255
      then error "Can only store 255 characters"
      else do
        let lenWord  = fromIntegral (L8.length s) :: Word8
            lenWBits = bsToBits (L.cons lenWord L.empty)
            bits     = lenWBits ++ bsToBits s 
            g'       = setData g (modifyLSBs (getData g) bits)
        output outPath (StegBox g')

dig :: FilePath -> IO String
dig binPath = do
  mg <- bsToSteg <$> L.readFile binPath
  case mg of 
    Nothing -> return ""
    Just (StegBox g, _) -> do
       let lsbs   = getLSBs $ getData g
           bitLen = 8 * binToDec (take 8 lsbs) 
       return $ reverse $ filter (/= '\'') $ boolsToStr (take bitLen (drop 8 lsbs))

msgLen :: Steg t => t -> Int
msgLen g = let lsbs = getLSBs $ getData g in
               binToDec $ take 8 lsbs 
                  
boolsToStr :: [Bool] -> String
boolsToStr bs = if length bs < 8 
                then ""
                else show (chr $ binToDec (take 8 bs)) ++ boolsToStr (drop 8 bs)

getLSBs :: L.ByteString -> [Bool]
getLSBs bs = let mw = L.uncons bs in
             case mw of
               Nothing -> []
               (Just (w', bs')) -> 
                   let eb = getLSB (B.pack [w']) in
                   case eb of
                       (Left s) -> error s
                       (Right b) -> b : getLSBs bs'

binToDec :: [Bool] -> Int
binToDec l = sum $ map (2^) $ elemIndices True $ reverse l
  
toStrict :: L.ByteString -> B.ByteString
toStrict = B.concat . L.toChunks

getLSB :: B.ByteString -> Either String Bool
getLSB bs = BG.runBitGet bs $ do
    BG.skip 7
    BG.getBit 

output :: FilePath -> StegBox -> IO ()
output path (StegBox s) = 
  L.writeFile path (L.concat $ [getHeader s, nl, getData s])
