module Main
    where

import           System.FilePath ((</>))
--import           Filesystem.Path (parent)
import           System.IO.Temp (withTempFile)
import           Control.Monad.IO.Class (liftIO)

import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Char (isAsciiLower)
import           Data.Maybe (fromJust)
import           Steg.Parse (dig, bury')
import           Test.QuickCheck (Property, quickCheck, (==>))
import           Test.QuickCheck.Monadic (assert, monadicIO, run)
--import           Distribution.TestSuite.QuickCheck

samplesPath :: FilePath
samplesPath = "/home/jb259/haskell/src/steg/etc/samples"

prop_codecRAWPGM :: String -> Property
prop_codecRAWPGM msg = not (null msg) && all isAsciiLower msg ==> monadicIO test
    where test = do let inPath  = samplesPath </> "pgm/RAW/surf.pgm"
                        outPath = "./surf2.pgm"
                    --s <- parent "."
                    --putStrLn s
                    run $ bury' inPath outPath (L8.toStrict $ L8.pack (msg++"\n"))
                    readMsg <- run $ dig outPath
                    liftIO $ putStrLn ("msg:" ++ msg)
                    liftIO $ putStrLn ("read:" ++ (fromJust readMsg))
                    assert $ msg == fromJust readMsg
                    {-withTempFile "." "./surf2.pgm." $ \tmpFile hFile ->
                      do run $ bury' inPath tmpFile (L8.toStrict $ L8.pack (msg++"\n"))
                         readMsg <- run $ dig tmpFile
                         assert $ msg == fromJust readMsg-}

prop_codecBMP :: String -> Property
prop_codecBMP msg = not (null msg) && all isAsciiLower msg ==> monadicIO test
    where test = do let inPath   = samplesPath </> "pgm/RAW/surf.pgm"
                        outPath  = "./duck2.bmp"
                    run $ bury' inPath outPath (L8.toStrict $ L8.pack (msg++"\n"))
                    readMsg <- run $ dig outPath
                    liftIO $ putStrLn ("msg:" ++ msg)
                    liftIO $ putStrLn ("read:" ++ (fromJust readMsg))
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
