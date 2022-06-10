module Parser
  ( parseProg
  , Op(..)
  , Program(..)
  )
  where

import           Text.Parsec
import           Text.Parsec.ByteString.Lazy
import           Data.ByteString.Lazy (ByteString)

data Op = IncP -- ^ Increment the byte pointer
        | DecP -- ^ Decrement the byte pointer
        | IncV -- ^ Increment the value pointed
        | DecV -- ^ Decrement the value pointed
        | Put -- ^ Write the byte
        | Get -- ^ Read to the byte
        | Loop [Op] -- ^ Loop
        deriving Show

type Program = [Op]

ops :: Parser Char
ops = char '>'
  <|> char '<'
  <|> char '+'
  <|> char '-'
  <|> char '.'
  <|> char ','

toOp :: Parser Op
toOp = translate <$> ops
  where
    translate '>' = IncP
    translate '<' = DecP
    translate '+' = IncV
    translate '-' = DecV
    translate '.' = Put
    translate ',' = Get
    translate _ = undefined

loop :: Parser Op
loop = Loop <$> between (char '[') (char ']') program

operation :: Parser Op
operation = loop <|> toOp

program :: Parser Program
program = skipMany space >> sepEndBy operation spaces

parseProg :: ByteString -> Either ParseError Program
parseProg = runParser (program <* eof) () ""
