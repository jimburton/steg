module Main
    where

import           System.FilePath ((</>))
import           System.Directory (removeFile)
import           System.IO (hClose)
import           System.IO.Temp (withTempFile, openTempFile)
import           Control.Applicative ((<$>))

import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Maybe (fromJust)
import           Steg.Parse (dig, bury', buryWithHandle)
import           Test.QuickCheck (Property, quickCheck)
import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Gen
import           Test.QuickCheck.Monadic (assert, monadicIO, run)
--import           Distribution.TestSuite.QuickCheck

samplesPath :: FilePath
samplesPath = "etc/samples"

genSafeChar :: Gen Char
genSafeChar = elements ['a'..'z']

genSafeString :: Gen String
genSafeString = listOf1 genSafeChar

newtype SafeString = SafeString { unwrapSafeString :: String }
    deriving Show

instance Arbitrary SafeString where
    arbitrary = SafeString <$> genSafeString

prop_codecRAWPGM :: SafeString -> Property
prop_codecRAWPGM (SafeString msg) = do let inPath  = samplesPath </> "pgm/RAW/surf.pgm"
                                       prop_codecAny msg inPath "tmp.png" 

prop_codecBMP :: SafeString -> Property
prop_codecBMP (SafeString msg) = do let inPath  = samplesPath </> "bmp/24bit/duck.bmp"
                                    prop_codecAny msg inPath "tmp.bmp"

prop_codecAny :: String -> String -> String -> Property
prop_codecAny msg inPath tmp = monadicIO test
    where test = do run $ bury' inPath tmp (L8.toStrict $ L8.pack (msg++"\n"))
                    readMsg <- run $ dig tmp
                    run $ removeFile tmp
                    assert $ msg == fromJust readMsg

runTests :: IO ()
runTests = do quickCheck prop_codecRAWPGM
              quickCheck prop_codecBMP

main :: IO ()
main = runTests

{-tests :: IO [Test]
tests = return $
        [ test "prop_codecRAWPGM" prop_codecRAWPGM
        , test "prop_codecBMP" prop_codecBMP ]
-}
