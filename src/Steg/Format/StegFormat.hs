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
    , signature)
    where
    
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8

{- | StegBox is a wrapper type that uses an existential type to
     hide the type of the value it carries around, but
     reveal its value. This provides some opportunities
     to write functions polymorphically that would otherwise be
     more clunky.
 -}
data StegBox = forall n. Steg n => StegBox n

{- | Steg is the type of image formats used by the program. -}
class Show t => Steg t where
    getData   :: t -> L.ByteString
    setData   :: t -> L.ByteString -> t
    getHeader :: t -> L.ByteString
    
{- | Format is an enumerated type used to identify what kind of 
     image fie is being parsed. 
-}
data Format = PGM | BMP

signature :: L8.ByteString
signature = L8.pack "# CREATOR: steg v0.1"
