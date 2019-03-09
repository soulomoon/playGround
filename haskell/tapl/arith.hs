module Parser where

import           Text.Parsec
import qualified Text.Parsec.Token             as Tok
import qualified Text.Parsec.Expr              as Ex
import           Text.Parsec.String             ( Parser )
import           Data.Functor.Identity

data Term =
    TmTrue
  | TmFalse
  | TmIf Term Term Term
  | TmZero
  | TmSucc Term
  | TmPred Term
  | TmIsZero Term deriving (Show)

langDef :: Tok.LanguageDef ()
langDef = Tok.LanguageDef
  { Tok.commentStart    = ""
  , Tok.commentEnd      = ""
  , Tok.commentLine     = ""
  , Tok.nestedComments  = True
  , Tok.identStart      = letter
  , Tok.identLetter     = alphaNum <|> oneOf "_'"
  , Tok.opStart         = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , Tok.opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , Tok.reservedNames   = []
  , Tok.reservedOpNames = []
  , Tok.caseSensitive   = True
  }

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser langDef

parens :: Parser a -> Parser a
parens = Tok.parens lexer

reserved, reservedOp :: String -> Parser ()
reserved = Tok.reserved lexer
reservedOp = Tok.reservedOp lexer

prefixOp :: String -> (a -> a) -> Ex.Operator String () Identity a
prefixOp s f = Ex.Prefix (reservedOp s >> return f)

table :: Ex.OperatorTable String () Identity Term
table =
  [[prefixOp "succ" TmSucc, prefixOp "pred" TmPred, prefixOp "zero?" TmIsZero]]

-- if/then/else
ifthen :: Parser Term
ifthen = do
  reserved "if"
  cond <- term
  reservedOp "then"
  tv <- term
  reserved "else"
  TmIf cond tv <$> term

-- Constants
true, false, zero :: Parser Term
true = reserved "true" >> return TmTrue
false = reserved "false" >> return TmFalse
zero = reservedOp "0" >> return TmZero

term :: Parser Term
term = Ex.buildExpressionParser table factor

factor :: Parser Term
factor = true <|> false <|> zero <|> ifthen <|> parens term

parseTerm :: String -> Either ParseError Term
parseTerm = parse term "name"

expr0, expr1, expr2 :: String
expr0 = "if (if zero? 0 then true else false) then true else false"
expr1 = "if false then 0 else succ (pred (succ 0))"
expr2 = "if true then succ 0 else -1"

ppt :: String -> IO ()
ppt s = do
  print "------"
  print $ parseTerm s

main :: IO ()
main = do
  ppt expr0
  ppt expr1
