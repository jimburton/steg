module Steg.Format.PGM where

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Char (isSpace)
import           Data.List (intersperse)
import           Steg.Format.StegFormat (Steg(..), StegBox(..))
import           Steg.Info (signature)

data PGMmap = PGMmap {
      pgmWidth :: Int
    , pgmHeight :: Int
    , pgmMax :: Int
    , pgmData :: L.ByteString
    } 

instance Show PGMmap where
    show (PGMmap w h m _) = "PGMmap " ++ show w ++ "x" ++ show h ++
                             " " ++ show m
instance Steg PGMmap where
    getData     = pgmData
    setData g d = g { pgmData = d }
    getHeader   = pgmHeader

pgmHeader :: PGMmap -> L.ByteString 
pgmHeader (PGMmap w h m d) = L.concat $ intersperse nl [magicNumber
                             , signature
                             , L8.pack $ show w ++ " " ++ show h
                             , L8.pack $ show m] 

magicNumber = L8.pack "P5"

nl = L8.cons '\n' L8.empty

parsePGM :: L.ByteString -> Maybe (StegBox, L.ByteString)
parsePGM s =
    matchHeader magicNumber s         >>=
    \s -> skipSpace ((), s)           >>=
    \(_, s) -> skipComment ((), s)    >>=
    (getNat . snd)                    >>=
    skipSpace                         >>=
    \(width, s) ->   getNat s         >>=
    skipSpace                         >>=
    \(height, s) ->  getNat s         >>=
    \(maxGrey, s) -> getBytes 1 s     >>=
    (getBytes (width * height) . snd) >>=
    \(bitmap, s) -> Just (StegBox (PGMmap width height maxGrey bitmap), s)

matchHeader :: L.ByteString -> L.ByteString -> Maybe L.ByteString
matchHeader prefix str
    | prefix `L8.isPrefixOf` str
        = Just (L8.dropWhile isSpace (L.drop (L.length prefix) str))
    | otherwise
        = Nothing

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

