module Parser where

import Text.Parsec
import Text.Parsec.ByteString.Lazy
import Data.ByteString.Lazy

data Op = While [Op]
        | DoWhile [Op]
        | Neg
        | Not
        | Tog
        | Diff
        | Xor
        | SwapTop
        | SwapThird
        | SwapAdj
        | RevZ
        | RevE
        | MoveHLeft
        | MoveHRight
        | TakeHLeft
        | TakeHRight
        | I
        | SwapL
        | SwapR
        | X deriving Show

type Program = [Op]

ops :: Parser Char
ops = char '-'
  <|> char '!'
  <|> char '*'
  <|> char '_'
  <|> char '^'
  <|> char ':'
  <|> char '+'
  <|> char '='
  <|> char '|'
  <|> char 'T'
  <|> char '<'
  <|> char '>'
  <|> char '['
  <|> char ']'
  <|> char 'I'
  <|> char '/'
  <|> char '\\'
  <|> char 'X'

toOp :: Parser Op
toOp = translate <$> ops
  where
    translate '-' = Neg
    translate '!' = Not
    translate '*' = Tog
    translate '_' = Diff
    translate '^' = Xor
    translate ':' = SwapTop
    translate '+' = SwapThird
    translate '=' = SwapAdj
    translate '|' = RevZ
    translate 'T' = RevE
    translate '<' = MoveHLeft
    translate '>' = MoveHRight
    translate '[' = TakeHLeft
    translate ']' = TakeHRight
    translate 'I' = I
    translate '/' = SwapL
    translate '\\' = SwapR
    translate 'X' = X
    translate  _  = undefined

whileLoop :: Parser Op
whileLoop = While <$> between (char '(') (char ')') program

doWhileLoop :: Parser Op
doWhileLoop = DoWhile <$> between (char '{') (char '}') program

operation :: Parser Op
operation = whileLoop <|> doWhileLoop <|> toOp

program :: Parser Program
program = skipMany space >> sepEndBy operation spaces

parseProg :: ByteString -> Either ParseError Program
parseProg = runParser (program <* eof) () ""
