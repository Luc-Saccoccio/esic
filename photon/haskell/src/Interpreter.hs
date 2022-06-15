module Interpreter where

import           Control.Monad.State.Strict
import           Data.Char                  (chr, digitToInt, intToDigit,
                                             isDigit, ord)
import qualified Data.Map.Strict            as M
import           Data.Vector                ((!?))
import           Parser

type Vars = M.Map Char Int
type Machine m a = StateT Vars m a

if' :: Bool -> a -> a -> a
if' True x _  = x
if' False _ y = y

varsInit :: Vars
-- varsInit = M.fromList [('$', 0)]
varsInit = M.singleton '$' 0

v :: Arg -> Vars -> Int
v (Lit n) _ = n
v (Var c) x = x M.! c
v Null _    = undefined

name :: Arg -> Char
name Null    = '-'
name (Var c) = c
name (Lit n) = intToDigit n

replaceRes :: (Int -> Int -> Int) -> Arg -> Arg -> Vars -> Vars
replaceRes f a b m = M.insert '$' (v a m `f` v b m) m

set :: Arg -> Arg -> Vars -> Vars
set a b m = M.insert (name a) (v b m) m

input :: Arg -> Machine IO ()
input a = do
  c <- liftIO getChar
  modify $ M.insert (name a) $ if' (isDigit c) digitToInt ord c

eval :: Program -> Int -> Machine IO ()
eval p n =
  case p !? n of
    Just (Add a b)    -> modify (replaceRes (+) a b) >> eval p (n+1)
    Just (Sub a b)    -> modify (replaceRes (-) a b) >> eval p (n+1)
    Just (Mul a b)    -> modify (replaceRes (*) a b) >> eval p (n+1)
    Just (Div a b)    -> modify (replaceRes div a b) >> eval p (n+1)
    Just (PrintV a)   -> (liftIO . print . v a =<< get) >> eval p (n+1)
    Just Rese         -> put varsInit >> eval p (n+1)
    Just (PrintA a)   -> (liftIO . putChar . chr . v a =<< get) >> eval p (n+1)
    Just PrintN       -> liftIO (putChar '\n') >> eval p (n+1)
    Just (Set a b)    -> modify (set a b) >> eval p (n+1)
    Just (Get a)      -> input a >> eval p (n+1)
    Just (Goto a)     -> get >>= eval p . v a
    Just (GotoIf a b) -> get >>= \m -> eval p (if v a m /= 0 then v b m else n+1)
    _                 -> return ()
