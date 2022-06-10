module Interpreter where

import           Control.Monad.State
import           Data.Char            (chr, ord)
import           Data.Int             (Int8)
import           Parser
import           Tape

type Machine m a = StateT (Tape Int8) m a

output :: Machine IO ()
output = do
  (Tape l x r) <- get
  liftIO . putChar . chr $ fromIntegral x

input :: Machine IO ()
input = do
  char <- liftIO $ fromIntegral . ord <$> getChar
  tape <- get
  put $ writeTape tape char

eval :: Program -> Machine IO ()
eval []        = return ()
eval (IncP:xs) = modify moveRightTape >> eval xs
eval (DecP:xs) = modify moveLeftTape >> eval xs
eval (IncV:xs) = modify incTape >> eval xs
eval (DecV:xs) = modify decTape >> eval xs
eval (Put:xs)  = output >> eval xs
eval (Get:xs)  = input >> eval xs
eval p@(Loop l:xs) = (\c -> if c == 0 then eval xs else eval l >> eval p) =<< gets readTape
