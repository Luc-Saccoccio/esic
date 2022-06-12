module Lib where

import Control.Monad.State
import Data.Char (chr)

data Op = Reset
        | Incr Int
        | Decr Int
        | Print

type Program = [Op]
type Machine m a = StateT Int m a

ops :: [Char]
ops = ['_', '+', '-', 'S']

parse :: String -> Program
parse = go
  where
    go :: String -> [Op]
    go ('+':xs) = incr 1 xs
    go ('-':xs) = decr 1 xs
    go ('_':xs) = Reset:go xs
    go ('S':xs) = Print:go xs
    go (_:xs) = go xs
    go [] = []

    incr :: Int -> String -> [Op]
    incr n [] = [Incr n]
    incr n ('+':xs) = incr (n+1) xs
    incr n (x:xs)
      | x `elem` ops = Incr n:go (x:xs)
      | otherwise = go xs

    decr :: Int -> String -> [Op]
    decr n [] = [Decr n]
    decr n ('-':xs) = decr (n+1) xs
    decr n (x:xs)
      | x `elem` ops = Decr n:go (x:xs)
      | otherwise = go xs

output :: Machine IO ()
output = get >>= liftIO . putChar . chr

eval :: Program -> Machine IO ()
eval [] = return ()
eval (Reset:xs) = modify (const 0) >> eval xs
eval (Incr n:xs) = modify (+n) >> eval xs
eval (Decr n:xs) = modify (subtract n) >> eval xs
eval (Print:xs) = output >> eval xs
