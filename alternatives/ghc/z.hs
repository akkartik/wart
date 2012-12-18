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

data Data = String String
          | Number Integer
          | Symbol String
          | Operator String
          | List [Data]
          deriving (Show, Eq)

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

isOperatorChar :: Char -> Bool
isOperatorChar x
  | isSpace x = False
  | isAlphaNum x = False
  | elem x punctuationChars = False
  | elem x symbolChars = False
  | otherwise = True
operator :: Parser Data
operator = liftM Operator (many1 $ satisfy isOperatorChar)

list :: Parser Data
list = do char '('
          elems <- sepBy expr whitespace
          char ')'
          return $ List elems

expr :: Parser Data
expr = string2
   <|> number
   <|> symbol
   <|> operator
   <|> (try list)

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

join sep "" b = b
join sep a b = a++sep++b

showVal :: Data -> String
showVal (String contents) = "\""++contents++"\""
showVal (Number contents) = show contents
showVal (Symbol name) = "sym "++name
showVal (Operator name) = "`"++name++"`"
showVal (List elems) = "["++(foldl (join ", ") "" (map showVal elems))++"]"

readExpr :: String -> String
readExpr input =
  case parse expr "wart" input of
    Left err -> show err
    Right val -> showVal val

repl = do line <- getLine
          putStr "=> "
          putStrLn $ readExpr line
          repl

main :: IO ()
main = do putStrLn "ready! type in an expr all on one line. ctrl-d exits."
          repl
