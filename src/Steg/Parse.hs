module Steg.Parse
    (dig, bury, bury')
    where

import           Control.Applicative ((<$>))
import qualified Data.Binary.Strict.BitGet as BG
import           Data.Bits (setBit, clearBit)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Char (chr)
import           Data.List (elemIndices)
import           Data.Word8 (Word8)
import           Steg.Format.BMP
import           Steg.Format.PGM
import           Steg.Format.StegFormat (Steg(..)
                                        , StegBox(..)
                                        , Format(..)
                                        , magicNumbers)

setLSB :: Bool -> Word8 -> Word8
setLSB b w = if b then setBit w 0 else clearBit w 0

wordToBits :: Integral a => a -> [a]
wordToBits = pad . reverse . decToBin 
    where pad xs     = replicate (8 - length xs) 0 ++ xs
          decToBin 0 = []
          decToBin y = let (a,b) = quotRem y 2 in b : decToBin a

bsToBits :: B.ByteString -> [Word8]
bsToBits = bsToBits' []
    where bsToBits' xs bs =  
            let h = B.uncons bs in
            case h of
              Nothing -> xs
              Just (w, bs') -> bsToBits' (wordToBits w ++ xs) bs'

modifyLSBs :: B.ByteString -> [Word8] -> B.ByteString
modifyLSBs bs []     = bs
modifyLSBs bs (w:ws) = 
    let mw = B.uncons bs in
    case mw of
      Nothing -> B.empty
      (Just (w', bs')) -> B.cons (setLSB (w==1) w') (modifyLSBs bs' ws) 

bsToSteg :: B.ByteString -> Maybe StegBox
bsToSteg bs = 
    case idHeader bs of
      Just PGM -> parsePGM bs
      Just BMP -> parseBMP bs
      Nothing  -> Nothing
                      
idHeader :: B.ByteString -> Maybe Format
idHeader bs = let mn = L8.unpack $ L8.fromChunks [B.take 2 bs] in
              lookup mn magicNumbers

bury :: FilePath -> FilePath -> FilePath -> IO ()
bury inPath txtPath outPath = B.readFile txtPath >>= bury' inPath outPath

bury' :: FilePath -> FilePath -> B.ByteString ->IO ()
bury' inPath outPath bs = 
    if B.length bs > 255
    then error "Can only store 255 characters"
    else do
      mg <- bsToSteg <$> B.readFile inPath
      case mg of
        Nothing -> return ()
        Just (StegBox g) -> 
            do
              let bs'      = L8.toStrict $ L8.filter (/='\n') (L8.fromChunks [bs])
                  lenWord  = fromIntegral (B.length bs') :: Word8
                  lenWBits = bsToBits (B.cons lenWord B.empty)
                  bits     = lenWBits ++ bsToBits bs' 
                  g'       = setData g (modifyLSBs (getData g) bits)
              output outPath $ StegBox g'

output :: FilePath -> StegBox -> IO ()
output path (StegBox s) = 
  B.writeFile path $ sGetContents s

dig :: FilePath -> IO (Maybe String)
dig binPath = do
  mg <- bsToSteg <$> B.readFile binPath
  case mg of 
    Nothing -> return Nothing
    Just (StegBox g) -> do
       let lsbs   = getLSBs $ getData g
           bitLen = 8 * binToDec (take 8 lsbs)
           result = reverse $ filter (\c -> c/='\'' && c/='\n') $ 
                       boolsToStr (take bitLen (drop 8 lsbs))
       return $ Just result

boolsToStr :: [Bool] -> String
boolsToStr bs = if length bs < 8 
                then ""
                else show (chr $ binToDec (take 8 bs)) ++ boolsToStr (drop 8 bs)

getLSBs :: B.ByteString -> [Bool]
getLSBs bs = let mw = B.uncons bs in
             case mw of
               Nothing -> []
               (Just (w', bs')) -> 
                   let eb = getLSB (B.pack [w']) in
                   case eb of
                       (Left s) -> error s
                       (Right b) -> b : getLSBs bs'

binToDec :: [Bool] -> Int
binToDec l = sum $ map (2^) $ elemIndices True $ reverse l
  
getLSB :: B.ByteString -> Either String Bool
getLSB bs = BG.runBitGet bs $ do
    BG.skip 7
    BG.getBit 
