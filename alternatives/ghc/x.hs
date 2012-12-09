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

{- A CSV file contains 0 or more lines, each of which is terminated
   by the end-of-line character (eol). -}
csvFile :: GenParser Char st [[String]]
csvFile =
    do result <- many line
       eof
       return result

-- Each line contains 1 or more cells, separated by a comma
line :: GenParser Char st [String]
line =
    do result <- cells
       eol                       -- end of line
       return result

-- Build up a list of cells.  Try to parse the first cell, then figure out
-- what ends the cell.
cells :: GenParser Char st [String]
cells =
    do first <- quotedCell <|> cellContent
       next <- remainingCells
       return (first : next)

-- The cell either ends with a comma, indicating that 1 or more cells follow,
-- or it doesn't, indicating that we're at the end of the cells for this line
remainingCells :: GenParser Char st [String]
remainingCells =
    (char ',' >> cells)            -- Found comma?  More cells coming
    <|> (return [])                -- No comma?  Return [], no more cells

-- Each cell contains 0 or more characters, which must not be a comma or
-- EOL
cellContent :: GenParser Char st String
cellContent =
    many (noneOf ",\n")

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
      ([["hi"]], ["hi\n"])
    , ([["hi", "there"]], ["hi,there\n"])
    , ([["hi"], [""], ["there"]], ["hi\n\nthere\n"])
    , ([["a, b"]], ["\"a, b\"\n"])
    , ([["a, \"b"]], ["\"a, \"\"b\"\n"])
    ]
, negativeCases = []
}
