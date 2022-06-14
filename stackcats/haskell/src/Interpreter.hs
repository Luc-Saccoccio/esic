module Interpreter where

import           Control.Monad.State.Lazy
import           Parser                   (Op (..), Program (..))
import           Tape
import Debug.Trace (trace)

type Machine = State StackCats (Stream (Maybe Int))

doWhile :: Program -> Int -> Machine
doWhile l n =
  eval l >> trace "x\n" (gets topValue >>= \v -> if v /= n then doWhile l n else gets (\(Tape _ x _) -> x))

eval :: Program -> Machine
eval []               = (\(Tape _ x _) -> x) <$> get
eval (Neg:xs)         = modify neg >> eval xs
eval (Not:xs)         = modify bitNot >> eval xs
eval (Tog:xs)         = modify toggle >> eval xs
eval (Diff:xs)        = modify diff >> eval xs
eval (Xor:xs)         = modify xorS >> eval xs
eval (SwapTop:xs)     = modify swapTop >> eval xs
eval (SwapThird:xs)   = modify swapThird >> eval xs
eval (SwapAdj:xs)     = modify swapAdj >> eval xs
eval (RevZ:xs)        = modify revStreamUpTo >> eval xs
eval (RevE:xs)        = modify revWholeStream >> eval xs
eval (MoveHLeft:xs)   = modify moveL >> eval xs
eval (MoveHRight:xs)  = modify moveR >> eval xs
eval (TakeHLeft:xs)   = modify takeL >> eval xs
eval (TakeHRight:xs)  = modify takeR >> eval xs
eval (I:xs)           = modify i >> eval xs
eval (SwapL:xs)       = modify swapL >> eval xs
eval (SwapR:xs)       = modify swapR >> eval xs
eval (X:xs)           = modify x >> eval xs
eval p@(DoWhile l:xs) = gets topValue >>= doWhile l >> eval xs
eval p@(While l:xs)  =
  (\c -> if c <= 0 then eval xs else eval l >> eval p) =<< gets topValue
