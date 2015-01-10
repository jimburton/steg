-- | 
-- | Module      :  Main.hs
-- | Description :  Entry point for the steg program
-- | Copyright   :  (c) Jim Burton
-- | License     :  MIT
-- | 
-- | Maintainer  :  j.burton@brighton.ac.uk
-- | Stability   :  provisional 
-- | Portability :  portable 
-- | 
module Main
    where

import Data.Maybe (fromJust)
import Steg.Parse (dig, bury)
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode (ExitFailure))

-- | Lookup table mapping command-line options to functions. 
dispatch :: [(String, [String] -> IO ())]  
dispatch =  [ ("bury", buryAct)  
            , ("dig", digAct)  
            ]  


-- | Bury some text. 
buryAct :: [String] -> IO ()
buryAct (inP:msgP:outP:_) = bury inP msgP outP
buryAct _                 = usageAndExit
  
-- | Dig some text. 
digAct :: [String] -> IO ()
digAct (inP:_) = dig inP >>= putStrLn . fromJust
digAct _       = usageAndExit

usage :: String
usage = "steg v.0.1 \n\
\-------------      \n\
\usage: steg bury imageIn txtFile imageOut \n\
\       steg dig image"

usageAndExit :: IO ()
usageAndExit = putStrLn usage >> exitWith (ExitFailure 1)

-- | The entry point for the program. 
main :: IO ()
main = do
  args <- getArgs
  if null args
  then usageAndExit
  else case lookup (head args) dispatch of
         (Just action) -> action (tail args)
         Nothing       -> usageAndExit
