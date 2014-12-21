module Steg.Format.PGM where

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Char (isSpace)
import           Data.List (intersperse)
import           Steg.Format.StegFormat (Steg, StegFormat(PGMmap))
import           Steg.Info (signature)

magicString = L8.pack "P5"

parsePGM :: L.ByteString -> Maybe (StegFormat, L.ByteString)
parsePGM s =
    matchHeader magicString s         >>=
    \s -> skipSpace ((), s)           >>=
    \(_, s) -> skipComment ((), s)    >>=
    (getNat . snd)                    >>=
    skipSpace                         >>=
    \(width, s) ->   getNat s         >>=
    skipSpace                         >>=
    \(height, s) ->  getNat s         >>=
    \(maxGrey, s) -> getBytes 1 s     >>=
    (getBytes (width * height) . snd) >>=
    \(bitmap, s) -> Just (PGMmap width height maxGrey bitmap, s)

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

outputPGM :: FilePath -> StegFormat -> IO ()
outputPGM path (PGMmap w h m d) = 
  L.writeFile path (L.concat $ intersperse nl [magicString
                             , signature
                             , L8.pack $ show w ++ " " ++ show h
                             , L8.pack $ show m
                             , d])
    where nl = L8.cons '\n' L8.empty 
