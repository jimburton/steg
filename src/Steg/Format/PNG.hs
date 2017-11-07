-- |
-- | Module      :  PNG.hs
-- | Description :  Parsing Portable Network Grahics (PNG) files
-- | Copyright   :  (c) Jim Burton
-- | License     :  MIT
-- |
-- | Maintainer  :  j.burton@brighton.ac.uk
-- | Stability   :  provisional
-- | Portability :  portable
-- |

module Steg.Format.PNG
    where

import qualified Codec.Picture.Png   as Png
import           Codec.Picture.Types (DynamicImage)

-- | The type of parsed PNGs
data PNGmap = PNGmap {
      pngHeader :: B.ByteString
      , pngData :: B.ByteString
      , thePNG  :: DynamicImage
      }

instance Steg PNGmap where
    getData      = pngData
    setData p d  = let innerPNG = thePNG b in
                  p {pngData = d 
                    , thePNG = innerPNG { BMP.bmpRawImageData = d } }
    getHeader    = pngHeader
    sGetContents = undefined
