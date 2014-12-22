module TestSteg
    where

import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Char (isAlphaNum)
import           Data.Maybe (fromJust)
import           Steg.Parse (dig, bury')
import           Test.QuickCheck (Property, quickCheck, (==>))
import           Test.QuickCheck.Monadic (assert, monadicIO, run)

prop_codecRAWPGM :: String -> Property
prop_codecRAWPGM msg = not (null msg) && all isAlphaNum msg ==> monadicIO test
    where test = do let inPath  = "/home/jb259/tmp/surf.pgm"
                        outPath  = "/home/jb259/tmp/surf2.pgm"
                    run $ bury' inPath outPath (L8.toStrict $ L8.pack (msg++"\n"))
                    readMsg <- run $ dig outPath
                    assert $ msg == fromJust readMsg

prop_codecBMP :: String -> Property
prop_codecBMP msg = not (null msg) && all isAlphaNum msg ==> monadicIO test
    where test = do let inPath  = "/home/jb259/tmp/duck.bmp"
                        outPath  = "/home/jb259/tmp/duck2.bmp"
                    run $ bury' inPath outPath (L8.toStrict $ L8.pack (msg++"\n"))
                    readMsg <- run $ dig outPath
                    assert $ msg == fromJust readMsg

runTests :: IO ()
runTests = do quickCheck prop_codecRAWPGM
              quickCheck prop_codecBMP

main :: IO ()
main = runTests
