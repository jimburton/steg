{- |
Module      :  Main.hs
Description :  Entry point for the steg program
Copyright   :  (c) Jim Burton
License     :  MIT

Maintainer  :  j.burton@brighton.ac.uk
Stability   :  provisional 
Portability :  portable 

-}
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
buryAct args = do
  let (imgPath:txtPath:outPath:_) = args
  bury imgPath txtPath outPath
  
-- | Dig some text. 
digAct :: [String] -> IO ()
digAct (imgPath:_) = dig imgPath >>= putStrLn . fromJust

-- | The entry point for the program. 
main :: IO ()
main = do
  args <- getArgs
  if null args 
  then exitWith (ExitFailure 1)
  else do let cmd = head args
              mAct = lookup cmd dispatch  
          case mAct of
            (Just action) -> action (tail args)
            Nothing -> putStrLn "Unknown argument"
  
