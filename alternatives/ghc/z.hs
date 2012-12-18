module Main where
import Control.Monad hiding (join)
import Data.Char
import Data.List
import System.IO
import System.Environment
import Test.HUnit
import Test.HUnit.Parsec
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char

main = do putStrLn "ready! type in an expr all on one line. ctrl-d exits."
          repl

repl = do line <- getLine
          putStr "=> "
          putStrLn $ readExpr line
          repl

readExpr :: String -> String
readExpr input =
  case parse expr "wart" input of
    Left err -> show err
    Right val -> showData val

data Data = String String
          | Number Integer
          | Symbol String
          | Operator String
          | List [Data]
          deriving (Show, Eq)

showData :: Data -> String
showData (String contents) = "\""++contents++"\""
showData (Number contents) = show contents
showData (Symbol name) = "sym "++name
showData (Operator name) = "`"++name++"`"
showData (List elems) = "["++(foldl (join ", ") "" (map showData elems))++"]"

-- parser

expr :: Parser Data
expr = string2
   <|> number
   <|> symbol
   <|> operator
   <|> (try list)

whitespace :: Parser ()
whitespace = skipMany1 space

quotedChar :: Parser Char
quotedChar = do char '\\'
                anyChar

string2 :: Parser Data
string2 = do char '"'
             x <- many (quotedChar <|> (noneOf "\""))
             char '"'
             return $ String x

number :: Parser Data
number = liftM (Number . read) (many1 digit)

punctuationChars = "()\"'`,@"

symbolChars = "$?!_"
symbol :: Parser Data
symbol = liftM Symbol (many1 $ letter <|> digit <|> oneOf symbolChars)

operator :: Parser Data
operator = liftM Operator (many1 $ satisfy isOperatorChar)

isOperatorChar :: Char -> Bool
isOperatorChar x
  | isSpace x = False
  | isAlphaNum x = False
  | elem x punctuationChars = False
  | elem x symbolChars = False
  | otherwise = True

list :: Parser Data
list = do char '('
          elems <- sepBy expr whitespace
          char ')'
          return $ List elems

test1 = ParsecTest {
  parser = expr
, initState = ()
, positiveCases = [
      (String "abc", ["\"abc\""])
    , (String "abc\"", ["\"abc\\\"\""])
    , (Number 34, ["34"])
    , (Symbol "$abc", ["$abc"])
    , (Operator "+<+", ["+<+"])
    , (List [(Symbol "a"), (Number 34)], ["(a 34)"])
    , (List [(Symbol "a"), (Number 34), (Operator "..."), (Symbol "b")], ["(a 34 ... b)"])
    ]
, negativeCases = []
}

-- misc

join sep "" b = b
join sep a b = a++sep++b
