module Main where

import           Control.Monad              (void)
import           Control.Monad.State.Strict (runStateT)
import qualified Data.ByteString.Lazy       as BS
import           Interpreter
import           Parser
import           System.Environment

main :: IO ()
main = do
  args <- getArgs
  content <- parseProg <$> BS.readFile (head args)
  case content of
    Left e -> print e
    Right p ->
      void $ runStateT (eval p 0) []
