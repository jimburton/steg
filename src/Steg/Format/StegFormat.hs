{-# LANGUAGE ExistentialQuantification #-}
{- |
Module      :  StegFormat.hs
Description :  Type class and data types for steg.
Copyright   :  (c) Jim Burton
License     :  MIT

Maintainer  :  j.burton@brighton.ac.uk
Stability   :  provisional 
Portability :  portable 

-}
module Steg.Format.StegFormat 
    (Steg(..)
    , StegBox(..)
    , Format(..)
    , signature
    , magicNumbers)
    where
    
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as L8

{- | StegBox is a wrapper type that uses an existential type to
     hide the type of the value it carries around, but
     reveal its value. This provides some opportunities
     to write functions polymorphically that would otherwise be
     more clunky.
 -}
data StegBox = forall n. Steg n => StegBox n

{- | Steg is the type of image formats used by the program. -}
class Steg t where
    getData   :: t -> B.ByteString
    setData   :: t -> B.ByteString -> t
    getHeader :: t -> B.ByteString
    sGetContents :: t -> B.ByteString
    
{- | Format is an enumerated type used to identify what kind of 
     image fie is being parsed. 
-}
data Format = PGM | BMP deriving (Show, Eq, Ord)

signature :: B.ByteString
signature = L8.toStrict $ L8.pack "# CREATOR: steg v0.1"

{- | Lookup from magic numbers to Formats. -}
magicNumbers :: [(String, Format)]
magicNumbers = [("P5", PGM), ("BM", BMP)]
