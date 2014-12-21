module Steg.Format.StegFormat 
    (Steg, StegFormat(..), getData, setData)
    where
    

import qualified Data.ByteString.Lazy as L

class Show t => Steg t where
    getData :: t -> L.ByteString
    setData :: t -> L.ByteString -> t

data StegFormat = 
    PGMmap {
      pgmWidth :: Int
    , pgmHeight :: Int
    , pgmMax :: Int
    , pgmData :: L.ByteString
    } 
                | 
    BMPmap {
      bmpData :: L.ByteString
    }  deriving (Eq)

instance Show StegFormat where
    show (PGMmap w h m _) = "PGMmap " ++ show w ++ "x" ++ show h ++
                             " " ++ show m
    show (BMPmap _)       = "BMPmap"

instance Steg StegFormat where
    getData PGMmap { pgmData = d } = d
    getData BMPmap { bmpData = d } = d
    setData g@PGMmap { } d = g { pgmData = d }
    setData b@BMPmap { } d = b { bmpData = d }


