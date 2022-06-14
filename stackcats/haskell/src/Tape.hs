module Tape where

import           Data.Bits     (complement, xor)
import           Data.Char     (chr)
import           Data.Function (fix)
import           Data.Maybe    (fromMaybe, isNothing)

data Stream a = a :> Stream a
infixr 0 :>
data Tape a = Tape (Stream a) a (Stream a)

streamOf :: a -> Stream a
streamOf x = fix (x :>)

type StackCats = Tape (Stream (Maybe Int))

v :: Maybe Int -> Maybe Int
v Nothing = Just 0
v x       = x

printStream :: Stream (Maybe Int) -> IO ()
printStream = putStrLn . accumulate
 where
  pmod :: Int -> Int
  pmod n = ((n `mod` 256) + 256) `mod` 256
  accumulate :: Stream (Maybe Int) -> String
  accumulate (Nothing              :> xs) = [] -- "0"
  accumulate (Just (-1) :> Nothing :> xs) = [] -- "-1; 0"
  accumulate (Just x               :> xs) = (chr $ pmod x):accumulate xs
  -- accumulate (Just x               :> xs) = (show x)++";"++accumulate xs

-- Arithmetic

neg :: StackCats -> StackCats -- GOOD
neg (Tape l (x :> xs) r) = Tape l (negate <$> v x :> xs) r

bitNot :: StackCats -> StackCats -- GOOD
bitNot (Tape l (x :> xs) r) = Tape l (complement <$> v x :> xs) r

toggle :: StackCats -> StackCats -- GOOD
toggle (Tape l (x :> xs) r) = Tape l ((`xor` 1) <$> v x :> xs) r

diff :: StackCats -> StackCats -- GOOD
diff (Tape l (x :> y :> xs) r) = Tape l ((-) <$> v y <*> v x :> y :> xs) r

xorS :: StackCats -> StackCats -- GOOD
xorS (Tape l (x :> y :> xs) r) = Tape l (xor <$> v y <*> v x :> y :> xs) r

-- Stack manipulation

push :: StackCats -> Int -> StackCats -- GOOD
push (Tape l x r) n = Tape l (Just n :> x) r

swapTop :: StackCats -> StackCats -- GOOD
swapTop (Tape l (x :> y :> xs) r) = Tape l (v y :> v x :> xs) r

swapThird :: StackCats -> StackCats -- GOOD
swapThird (Tape l (x :> y :> z :> xs) r) = Tape l (v z :> v y :> v x :> xs) r

swapAdj :: StackCats -> StackCats -- GOOD
swapAdj (Tape (l :> ls) x (r :> rs)) =
  let (l', r') = swap l r
   in Tape (l' :> ls) x (r' :> rs)
  where
    swap (x :> xs) (y :> ys) = (y :> xs, x :> ys)

append ::  [Maybe Int] -> Stream (Maybe Int) -> Stream (Maybe Int)
append xs s = foldr (:>) s xs

revStreamUpTo :: StackCats -> StackCats -- GOOD
revStreamUpTo (Tape l x r) =
  let (rev, rest) = get x []
   in Tape l (append rev rest) r
  where
    get :: Stream (Maybe Int) -> [Maybe Int] -> ([Maybe Int], Stream (Maybe Int))
    get (x :> xs) acc
      | x `elem` [Nothing, Just 0] = (acc, x :> xs)
      | otherwise = get xs (x:acc)

revWholeStream :: StackCats -> StackCats -- GOOD
revWholeStream (Tape l x r) =
  let (rev, rest) = get x []
   in Tape l (append rev rest) r
  where
    get :: Stream (Maybe Int) -> [Maybe Int] -> ([Maybe Int], Stream (Maybe Int))
    get (x :> xs) acc
      | isNothing x = (acc, x :> xs)
      | otherwise = get xs (x:acc)

-- Movement and tape manipulation

moveL :: StackCats -> StackCats -- GOOD
moveL (Tape (l :> ls) x r) = Tape ls l (x :> r)

moveR :: StackCats -> StackCats -- GOOD
moveR (Tape l x (r :> rs)) = Tape (x :> l) r rs

takeL :: StackCats -> StackCats -- GOOD
takeL (Tape (l :> ls) (x :> xs) r) = Tape ls (x :> l) (xs :> r)

takeR :: StackCats -> StackCats -- GOOD
takeR (Tape l (x :> xs) (r :> rs)) = Tape (xs :> l) (x :> r) rs

i :: StackCats -> StackCats -- GOOD
i t@(Tape l (x :> xs) r)
  | any (<0) x = neg $ takeL t
  | any (>0) x = neg $ takeR t
  | otherwise = t

swapL :: StackCats -> StackCats
swapL (Tape (l :> ls) x r) = Tape ls x (l :> r)

swapR :: StackCats -> StackCats
swapR (Tape l x (r :> rs)) = Tape (r :> l) x rs

x :: StackCats -> StackCats -- GOOD
x (Tape l x r) = Tape r x l

topValue :: StackCats -> Int
topValue (Tape _ (x :> xs) _) = fromMaybe 0 x

blankTape :: StackCats
blankTape =
  let zeros = streamOf Nothing
  in  Tape (streamOf zeros) (Just (-1) :> zeros) (streamOf zeros)
