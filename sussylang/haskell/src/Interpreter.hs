module Interpreter where

import           Control.Monad.State.Strict
import           Data.Char                  (chr, digitToInt, intToDigit, ord)
import           Data.Vector                ((!?))
import           Parser

type Machine m a = StateT [Int] m a

outputI :: Machine IO ()
outputI = gets head >>= liftIO . putStr . show

outputC :: Machine IO ()
outputC = gets head >>= liftIO . putChar . chr

inputI :: Machine IO ()
inputI = do
  char <- liftIO $ digitToInt <$> getChar
  modify (char:)

inputC :: Machine IO ()
inputC = do
  char <- liftIO $ ord <$> getChar
  modify (char:)

eval :: Program -> Int -> Machine IO ()
eval p n =
  case p !? n of
    Just (Push m) -> modify (m:) >> eval p (n+1)
    Just Pop      -> modify tail >> eval p (n+1)
    Just Print    -> outputI >> eval p (n+1)
    Just PrintC   -> outputC >> eval p (n+1)
    Just Neg      -> modify (\(x:xs) -> (negate x:xs)) >> eval p (n+1)
    Just PopS     -> modify (\(x:y:xs) -> (x+y:xs)) >> eval p (n+1)
    Just PopM     -> modify (\(x:y:xs) -> (x*y:xs)) >> eval p (n+1)
    Just PopD     -> modify (\(x:y:xs) -> (x`div`y:xs)) >> eval p (n+1)
    Just Swap     -> modify (\(x:y:xs) -> (y:x:xs)) >> eval p (n+1)
    Just Dup      -> modify (\(x:xs) -> (x:x:xs)) >> eval p (n+1)
    Just Skip     -> gets head >>= \m -> if m < 1 then eval p (n+2) else eval p (n+1)
    Just (Jump m) -> eval p m
    Just GetI    -> inputI >> eval p (n+1)
    Just GetC -> inputC >> eval p (n+1)
    _ -> return () -- Just Quit or Nothing
