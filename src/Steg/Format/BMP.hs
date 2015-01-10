-- |
-- | Module      :  BMP.hs
-- | Description :  Parsing Windows Bitmap (BMP) files
-- | Copyright   :  (c) Jim Burton
-- | License     :  MIT
-- | 
-- | Maintainer  :  j.burton@brighton.ac.uk
-- | Stability   :  provisional 
-- | Portability :  portable 
-- | 
module Steg.Format.BMP
    where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as L8
import           Steg.Format.StegFormat (Steg(..)
                                        , StegBox(..))
import           qualified Codec.BMP as BMP

-- | The type of parsed BMPs
data BMPmap = BMPmap {
      bmpHeader :: B.ByteString
      , bmpData :: B.ByteString
      , theBMP  :: BMP.BMP
    }

instance Steg BMPmap where
    getData     = bmpData
    setData b d = let innerBMP = theBMP b in
                  b { bmpData = d 
                    , theBMP = innerBMP { BMP.bmpRawImageData = d } }
    getHeader   = bmpHeader
    sGetContents = L8.toStrict . BMP.renderBMP . theBMP

-- | Parse a BMP from a ByteString. 
parseBMP :: B.ByteString -> Maybe StegBox
parseBMP bs = case BMP.parseBMP (L8.fromChunks [bs]) of
                Left _    -> Nothing
                Right pic -> do
                  let body   = BMP.bmpRawImageData pic
                      size   = BMP.fileHeaderFileSize $ BMP.bmpFileHeader pic
                      header = B.take (fromIntegral size) bs 
                  Just $ StegBox (BMPmap header body pic)
