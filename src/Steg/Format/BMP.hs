module Steg.Format.BMP
    where

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import           Steg.Format.StegFormat (Steg(..), StegBox(..))

data BMPmap = BMPmap {
      bmpHeader :: L.ByteString
      , bmpData  :: L.ByteString
    }

instance Show BMPmap where
    show (BMPmap _ _)       = "BMPmap"

instance Steg BMPmap where
    getData     = bmpData
    setData b d = b { bmpData = d }
    getHeader   = bmpHeader

parseBMP :: L.ByteString -> Maybe (StegBox, L8.ByteString)
parseBMP = undefined
