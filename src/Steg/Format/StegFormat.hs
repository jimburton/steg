{-# LANGUAGE ExistentialQuantification #-}
module Steg.Format.StegFormat 
    (Steg(..)
    , StegBox(..))
    where
    
import qualified Data.ByteString.Lazy as L

--data StegBox t = StegBox t
data StegBox = forall n. Steg n => StegBox n

class Show t => Steg t where
    getData   :: t -> L.ByteString
    setData   :: t -> L.ByteString -> t
    getHeader :: t -> L.ByteString
    
