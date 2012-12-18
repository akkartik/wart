module Main where
import System.Environment
import Data.Char
import Data.List
import Control.Monad
import Text.ParserCombinators.Parsec

data Data = String String
          | Number Integer
          | Symbol String
          | Operator String
          | List [Data]
          | DottedList [Data] Data

whitespace :: Parser ()
whitespace = skipMany1 space

string2 :: Parser Data
string2 = do char '"'
             x <- many (noneOf "\"")
             char '"'
             return $ String x

number :: Parser Data
number = liftM Number (many1 digit)

symbolChars = "$?!_"
symbol :: Parser Data
symbol = liftM String (many1 $ letter <|> digit <|> oneOf symbolChars)

isOperatorChar :: Char -> Bool
isOperatorChar x
  | isSpace x = False
  | isAlphaNum x = False
  | elem x symbolChars = False
  | otherwise = True
operator :: Parser Data
operator = liftM Operator (many1 $ satisfy isOperatorChar)

expr :: Parser Data
expr = string2
   <|> number
   <|> symbol
   <|> operator

showVal :: Data -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Number contents) = show contents
showVal (Symbol name) = name
showVal (Operator name) = name

readExpr :: String -> String
readExpr input =
  case parse expr "lisp" input of
    Left err -> show err
    Right val -> "Found value"

main :: IO ()
main = do args <- getArgs
          putStrLn $ readExpr $ args!!0
