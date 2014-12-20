module Main
    where

import           Control.Applicative ((<$>))
import qualified Data.Binary.Strict.BitGet as BG
import           Data.Bits (setBit, clearBit)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Char (isSpace, chr, isDigit)
import           Data.Int (Int64)
import           Data.List (intersperse, findIndices)
import           Data.Word (Word8)
import           Data.Word8 (_cr)

data Greymap = Greymap {
      greyWidth :: Int
    , greyHeight :: Int
    , greyMax :: Int
    , greyData :: L.ByteString
    } deriving (Eq)

instance Show Greymap where
    show (Greymap w h m _) = "Greymap " ++ show w ++ "x" ++ show h ++
                             " " ++ show m

matchHeader :: L.ByteString -> L.ByteString -> Maybe L.ByteString
matchHeader prefix str
    | prefix `L8.isPrefixOf` str
        = Just (L8.dropWhile isSpace (L.drop (L.length prefix) str))
    | otherwise
        = Nothing

-- "nat" here is short for "natural number"
getNat :: L.ByteString -> Maybe (Int, L.ByteString)
getNat s = case L8.readInt s of
             Nothing -> Nothing
             Just (num,rest)
                 | num <= 0    -> Nothing
                 | otherwise   -> Just (fromIntegral num, rest)

getBytes :: Int -> L.ByteString
         -> Maybe (L.ByteString, L.ByteString)
getBytes n str = let count           = fromIntegral n
                     both@(prefix,_) = L.splitAt count str
                 in if L.length prefix < count
                    then Nothing
                    else Just both

skipSpace :: (a, L.ByteString) -> Maybe (a, L.ByteString)
skipSpace (a, s) = Just (a, L8.dropWhile isSpace s)

skipComment :: (a, L.ByteString) -> Maybe (a, L.ByteString)
skipComment (a, s) = let s' = if L8.head s == '#'
                              then L.tail $ snd $ L8.span (/= '\n') s
                              else s
                     in Just (a, s')

parsePGM :: L.ByteString -> Maybe (Greymap, L.ByteString)
parsePGM s =
    matchHeader magicString s      >>=
    \s -> skipSpace ((), s)           >>=
    \(_, s) -> skipComment ((), s)    >>=
    (getNat . snd)                    >>=
    skipSpace                         >>=
    \(width, s) ->   getNat s         >>=
    skipSpace                         >>=
    \(height, s) ->  getNat s         >>=
    \(maxGrey, s) -> getBytes 1 s     >>=
    (getBytes (width * height) . snd) >>=
    \(bitmap, s) -> Just (Greymap width height maxGrey bitmap, s)

countWords :: Greymap -> Int64
countWords Greymap { greyData = d } = L.length d

signature = L8.pack "# CREATOR: steg v0.1"
magicString = L8.pack "P5"

outputPGM :: FilePath -> Greymap -> IO ()
outputPGM path (Greymap w h m d) = do
  L.writeFile path (L.concat $ intersperse nl [magicString
                             , signature
                             , L8.pack ((show w) ++ " " ++ (show h))
                             , L8.pack (show m)
                             , d])
    where nl = L8.cons '\n' L8.empty 

setLSB :: Bool -> Word8 -> Word8
setLSB True  w = setBit w 0
setLSB False w = clearBit w 0

wordToBits :: Integral a => a -> [a]
wordToBits = pad . reverse . decToBin 
    where pad xs     = (replicate (8 - length xs) 0) ++ xs
          decToBin 0 = []
          decToBin y = let (a,b) = quotRem y 2 in [b] ++ decToBin a
                 

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

buryText :: Greymap -> String -> Greymap
buryText g s = if length s > 255
               then error "Can only store 255 characters"
               else let lenWord  = fromIntegral (length s) :: Word8
                        lenWBits = bsToBits (L.cons lenWord L.empty)
                        bits = lenWBits ++ (bsToBits $ L8.pack s) in
                    g { greyData = (modifyLSBs (greyData g) bits) }

digupText :: Greymap -> String
digupText g = let lsbs   = getLSBs (greyData g) 
                  bitLen = 8 * (binToDec (take 8 lsbs)) in
              reverse $ filter (/= '\'') $ boolsToStr (take bitLen (drop 8 lsbs))

msgLen :: Greymap -> Int
msgLen g = let lsbs = getLSBs (greyData g) in
               binToDec $ take 8 lsbs 
                  
boolsToStr :: [Bool] -> String
boolsToStr bs = if length bs < 8 
                then ""
                else (show $ chr $ binToDec (take 8 bs)) ++ (boolsToStr (drop 8 bs))

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
binToDec l = sum $ map (2^) $ findIndices (==True) $ reverse l
  
toStrict :: L.ByteString -> B.ByteString
toStrict = B.concat . L.toChunks

getLSB :: B.ByteString -> Either String Bool
getLSB bs = BG.runBitGet bs ( do
    BG.skip 7
    BG.getBit )


