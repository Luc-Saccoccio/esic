module Main where

import           Control.Monad        (void)
import           Control.Monad.State  (runState)
import qualified Data.ByteString.Lazy as BS
import           Data.Char            (ord, digitToInt)
import           Data.List            (foldl')
import           Interpreter
import           Parser
import           System.Environment
import           Tape

evaluate :: Program -> String -> IO ()
evaluate p s =
  putStr "\nUnderstood: " >> print s >>
  let initial = foldl' push blankTape (map digitToInt $ reverse s)
   in printStream . fst $ runState (eval p) initial

main :: IO ()
main = do
  args <- getArgs
  content <- parseProg <$> BS.readFile (head args)
  case content of
    Left e  -> print e
    Right p -> getLine >>= evaluate p
