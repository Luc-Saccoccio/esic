module Tape  where

import           Data.Char (chr)
import           Data.Int  (Int8)

data Tape a = Tape [a] a [a]

writeTape :: Tape a -> a -> Tape a
writeTape (Tape l _ r) x = Tape l x r

readTape :: Tape a -> a
readTape (Tape _ x _) = x

incTape :: Num a => Tape a -> Tape a
incTape (Tape l x r) = Tape l (x+1) r

decTape :: Num a => Tape a -> Tape a
decTape (Tape l x r) = Tape l (x-1) r

moveRightTape :: Tape a -> Tape a
moveRightTape (Tape l x []) =
  let (rx:rxs) = reverse (x:l) in
      Tape [] rx rxs
moveRightTape (Tape l x (r:rs)) = Tape (x:l) r rs

moveLeftTape :: Tape a -> Tape a
moveLeftTape (Tape [] x r) =
  let (lx:lxs) = reverse (x:r) in
      Tape lxs lx []
moveLeftTape (Tape (l:ls) x r) = Tape ls l (x:r)

-- | Haskell gives us infinite array, hurray !
blankTape :: Tape Int8
blankTape = Tape [] 0 (repeat 0)
