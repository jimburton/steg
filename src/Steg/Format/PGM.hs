{- |
Module      :  PGM.hs
Description :  Code relating to parsing PGM files.
Copyright   :  (c) Jim Burton
License     :  MIT

Maintainer  :  j.burton@brighton.ac.uk
Stability   :  provisional 
Portability :  portable 

-}
module Steg.Format.PGM 
    (parsePGM)
    where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Char (isSpace)
import           Data.List (intersperse)
import           Steg.Format.StegFormat (Steg(..)
                                        , StegBox(..)
                                        , signature)

-- | The data type of PGM image files, with instance declarations 
-- |     for Show and Steg.

data PGMmap = PGMmap {
      pgmHeader :: B.ByteString
    , pgmData   :: B.ByteString
    } 

instance Steg PGMmap where
    getData     = pgmData
    setData g d = g { pgmData = d }
    getHeader g = pgmHeader g
    sGetContents g = B.concat [getHeader g, getData g]

nl :: B.ByteString
nl = L8.toStrict $ L8.cons '\n' L8.empty

magicPGM :: B.ByteString
magicPGM = L8.toStrict $ L8.pack "P5"

-- | Parse a PGM from a ByteString. 
parsePGM :: B.ByteString -> Maybe StegBox
parsePGM s =
    matchHeader magicPGM s  >>=
    \s1 -> skipSpace ((), s1)        >>=
    \(_, s2) -> skipComment ((), s2) >>=
    (getNat . snd)                   >>=
    skipSpace                        >>=
    \(w, s3) ->   getNat s3      >>=
    skipSpace                        >>=
    \(h, s4) ->  getNat s4      >>=
    \(m, s5) -> getBytes 1 s5  >>=
    (getBytes (w * h) . snd)  >>=
    \(bitmap, _) -> let header = B.concat $ init $ intersperse nl 
                                 [magicPGM
                                 , signature
                                 , L8.toStrict $ L8.pack $ show w ++ " " ++ show h
                                 , L8.toStrict $ L8.pack $ show m
                                 , B.empty] in
                    Just $ StegBox (PGMmap header bitmap)

matchHeader :: B.ByteString -> B.ByteString -> Maybe B.ByteString
matchHeader prefix str = let prefixL = L8.fromChunks [prefix]
                             strL    = L8.fromChunks [str] in
                         if prefixL `L8.isPrefixOf` strL then
                             Just (L8.toStrict $ 
                                     L8.dropWhile isSpace 
                                           (L8.drop (L8.length prefixL) strL))
                         else Nothing

getNat :: B.ByteString -> Maybe (Int, B.ByteString)
getNat bs = case L8.readInt (L8.fromChunks [bs]) of
              Nothing -> Nothing
              Just (num,rest)
                  | num <= 0    -> Nothing
                  | otherwise   -> Just (fromIntegral num, L8.toStrict rest)

getBytes :: Int -> B.ByteString
         -> Maybe (B.ByteString, B.ByteString)
getBytes n bs = let count           = fromIntegral n
                    both@(prefix,_) = B.splitAt count bs
                in if B.length prefix < count
                   then Nothing
                   else Just both

skipSpace :: (a, B.ByteString) -> Maybe (a, B.ByteString)
skipSpace (a, bs) = Just (a, L8.toStrict $ L8.dropWhile isSpace (L8.fromChunks [bs]))

skipComment :: (a, B.ByteString) -> Maybe (a, B.ByteString)
skipComment (a, bs) = let bs'  = L8.fromChunks [bs] 
                          bs'' = if L8.head bs' == '#'
                                 then L8.tail $ snd $ L8.span (/= '\n') bs'
                                 else bs'
                      in Just (a, L8.toStrict bs'')

