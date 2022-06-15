module Parser where

import           Control.Monad               (void)
import           Data.ByteString.Lazy        (ByteString)
import           Data.Char                   (digitToInt)
import           Data.Vector (Vector, fromList)
import           Text.Parsec
import           Text.Parsec.ByteString.Lazy
import           Text.Parsec.Char

data Arg = Lit Int | Var Char | Null

data Op = Add Arg Arg
        | Sub Arg Arg
        | Mul Arg Arg
        | Div Arg Arg
        | PrintV Arg
        | Rese
        | PrintA Arg
        | PrintN
        | Set Arg Arg
        | Get Arg
        | Goto Arg
        | GotoIf Arg Arg

type Program = Vector Op

lit :: Parser Arg
lit = do
  void $ char '#'
  c <- digitToInt <$> digit
  return (Lit c)

var :: Parser Arg
var = do
  void $ char ':'
  c <- anyChar
  return $ case c of
             '-' -> Null
             _   -> Var c

arg :: Parser Arg
arg = try lit <|> try var

operation :: Parser Op
operation = do
  arg1 <- arg
  void $ char ','
  arg2 <- arg
  void $ char '>'
  c <- ops
  return $ case c of
             '+' -> Add arg1 arg2
             '-' -> Sub arg1 arg2
             '*' -> Mul arg1 arg2
             '/' -> Div arg1 arg2
             '_' -> PrintV arg1
             '.' -> Rese
             '@' -> PrintA arg1
             '~' -> PrintN
             '=' -> Set arg1 arg2
             '?' -> Get arg1
             '^' -> Goto arg1
             '{' -> GotoIf arg1 arg2

ops :: Parser Char
ops = char '+'
  <|> char '-'
  <|> char '*'
  <|> char '/'
  <|> char '_'
  <|> char '.'
  <|> char '@'
  <|> char '~'
  <|> char '='
  <|> char '?'
  <|> char '^'
  <|> char '{'

command :: Parser Op
command = between (char '[') (char ']') operation

program :: Parser [Op]
program = skipMany space >> sepEndBy command spaces

parseProg :: ByteString -> Either ParseError Program
parseProg = (fromList <$>) . runParser (program <* eof) () ""
