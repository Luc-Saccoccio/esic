module Main where

import qualified Data.ByteString.Lazy as BS
import           Interpreter
import           Parser
import           System.Environment
import           Tape
import Control.Monad.State (runStateT)
import Control.Monad (void)

main :: IO ()
main = do
  args <- getArgs
  content <- parseProg <$> BS.readFile (head args)
  case content of
    Left e -> print e
    Right p ->
      void $ runStateT (eval p) blankTape
