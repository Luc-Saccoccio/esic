module Tape  where

import           Data.Char (chr)
import           Data.Int  (Int8)
import Data.Function (fix)

data Stream a = a :> Stream a

data Tape a = Tape (Stream a) a (Stream a)

writeTape :: Tape a -> a -> Tape a
writeTape (Tape l _ r) x = Tape l x r

readTape :: Tape a -> a
readTape (Tape _ x _) = x

incTape :: Num a => Tape a -> Tape a
incTape (Tape l x r) = Tape l (x+1) r

decTape :: Num a => Tape a -> Tape a
decTape (Tape l x r) = Tape l (x-1) r

moveRightTape :: Tape a -> Tape a
moveRightTape (Tape l x (r :> rs)) = Tape (x :> l) r rs

moveLeftTape :: Tape a -> Tape a
moveLeftTape (Tape (l :> ls) x r) = Tape ls l (x :> r)

streamOf :: a -> Stream a
streamOf x = fix (x :>)

blankTape :: Tape Int8
blankTape = Tape (streamOf 0) 0 (streamOf 0)
