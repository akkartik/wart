-- http://book.realworldhaskell.org/read/using-parsec.html
import Text.ParserCombinators.Parsec
import Test.HUnit
import Test.HUnit.Parsec

-- runhaskell x.hs < x.csv
main =
    do c <- getContents
       case parse csvFile "(stdin)" c of
            Left e -> do putStrLn "Error parsing input:"
                         print e
            Right r -> mapM_ print r

csvFile = sepBy line eol
line = sepBy cell (char ',')
cell = quotedCell <|> many (noneOf ",\n\r")

quotedCell =
    do char '"'
       content <- many quotedChar
       char '"' <?> "quote at end of cell"
       return content

quotedChar =
        noneOf "\""
    <|> try (string "\"\"" >> return '"')

eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "end of line"

parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(unknown)" input

test1 = ParsecTest {
  parser = csvFile
, initState = ()
, positiveCases = [
    ([["hi"]], ["hi"])
  , ([["hi", "there"]], ["hi,there"])
  , ([["hi"], [""], ["there"]], ["hi\n\nthere"])
  , ([["a, b"]], ["\"a, b\""])
  , ([["a, \"b"]], ["\"a, \"\"b\""])
  ]
, negativeCases = []
}
