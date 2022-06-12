module Main where

import Lib
import System.Environment
import Control.Monad.State (runStateT)
import Control.Monad (void)

main :: IO ()
main = do
  args <- getArgs
  content <- parse <$> readFile (head args)
  void $ runStateT (eval content) 0
