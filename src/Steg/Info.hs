module Steg.Info
    (Format(..), signature)
    where

import qualified Data.ByteString.Lazy.Char8 as L8

data Format = PGM | BMP

signature = L8.pack "# CREATOR: steg v0.1"
