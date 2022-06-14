{-# LANGUAGE TypeApplications #-}
module Parser where

import           Control.Monad               (void)
import           Data.ByteString.Lazy        (ByteString)
import           Data.Vector
import           Text.Parsec
import           Text.Parsec.ByteString.Lazy
import           Text.Parsec.Char

data Op = Push Int -- ^ Push an unisgnd integer to the front of the stack.
        | Pop -- ^ Pop the front of the stack.
        | Print -- ^ Print the front of the stack as an integer.
        | PrintC -- ^ Print using the front of the stack as the ASCII value of a character.
        | Neg -- ^ Negate the front of the stack.
        | PopS -- ^ Pop the two highest elements of the stack and pushes their sum to the stack.
        | PopM -- ^ Pop the two highest elements of the stack and pushes their product to the stack.
        | PopD -- ^ Pop the two highest elements of the stack and pushes their division to the stack.
        | Swap -- ^ Swap the two highest elements of the stack.
        | Dup -- ^ Duplicate the front of the stack.
        | Skip -- ^ If the front is 0 or less, skip the next line.
        | Jump Int -- ^ Jump to line
        | GetI -- ^ Gets an integer from user input.
        | GetC -- ^ Gets a character from user input.
        | Quit -- ^ Quits
        deriving Show

type Program = Vector Op

ops :: Parser String
ops = try (string "sussybaka")
  <|> try (string "sussyballs")
  <|> string "sussy"
  <|> string "amogus"
  <|> string "imposter"
  <|> string "crew"
  <|> string "ඞ"
  <|> string "amorgos"
  <|> string "amongdrip"
  <|> string "task"
  <|> string "redsus"
  <|> string "pensus"
  <|> string "emergencymeeting"


toOp :: Parser Op
toOp = translate <$> ops
  where
    translate "sussy"            = Pop
    translate "sussybaka"        = Print
    translate "amogus"           = PrintC
    translate "sussyballs"       = Neg
    translate "imposter"         = PopS
    translate "crew"             = PopM
    translate "ඞ"                = PopD
    translate "amorgos"          = Swap
    translate "amongdrip"        = Dup
    translate "task"             = Skip
    translate "redsus"           = GetI
    translate "pensus"           = GetC
    translate "emergencymeeting" = Quit

sus :: Parser Op
sus = do
  void $ string "sus"
  spaces
  n <- read @Int <$> many1 digit
  return $ Push n

jermasus :: Parser Op
jermasus = do
  void $ string "jermasus"
  skipMany1 space
  n <- read @Int <$> many1 digit
  return $ Jump n

operation :: Parser Op
operation = try sus <|> toOp <|> jermasus

program :: Parser [Op]
program = skipMany space >> sepEndBy operation spaces

parseProg :: ByteString -> Either ParseError Program
parseProg = (fromList <$>) . runParser (program <* eof) () ""
