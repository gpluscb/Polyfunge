{-# LANGUAGE NamedFieldPuns #-}

module Parse where

import Data.Char (digitToInt, isDigit, ord)
import ProgramData
import Utils

data ParseError = EmptyLiteral
  deriving (Read, Show, Eq, Enum)

parseProgram :: String -> Either ParseError ProgramState
parseProgram program =
  let rows = lines program
      startingValues = []
      startingBlocks = []
      parseResult = parseProgramRecursive 0 rows startingValues startingBlocks
   in uncurry constructProgramState <$> parseResult

parseProgramRecursive :: Int -> [String] -> [Value] -> [[Block]] -> Either ParseError ([Value], [[Block]])
parseProgramRecursive _ [] values blocks = Right (values, blocks)
parseProgramRecursive row (line : rest) values blocks =
  let lineParseResult = parseLine line row
      newProgramState =
        (\(rowValues, rowBlocks) -> (values ++ rowValues, blocks ++ [rowBlocks]))
          <$> lineParseResult
   in uncurry (parseProgramRecursive (succ row) rest) =<< newProgramState

parseLine :: String -> Int -> Either ParseError ([Value], [Block])
parseLine line row = parseLineRecursive (0, row) [] [] line

parseLineRecursive :: (Int, Int) -> [Value] -> [Block] -> String -> Either ParseError ([Value], [Block])
parseLineRecursive _ vals blocks "\n" = Right (vals, blocks)
parseLineRecursive _ vals blocks "" = Right (vals, blocks)
parseLineRecursive (col, row) vals blocks (' ' : rest) =
  parseLineRecursive (succ col, row) vals (blocks ++ [Util Default]) rest
parseLineRecursive (col, row) vals blocks ('\'' : c : rest) =
  let newValue =
        Value
          { position = (col, row),
            numericValue = ord c,
            momentum = DirDown,
            waiting = False
          }
      updateValues = push newValue vals
      updatedBlocks = blocks ++ replicate 2 (Util Default)
   in parseLineRecursive (col + 2, row) updateValues updatedBlocks rest
parseLineRecursive _ _ _ "'" = Left EmptyLiteral
parseLineRecursive (col, row) vals blocks ('(' : rest) =
  let parseDigitsResult = parseLargeNumber rest
   in parseDigitsResult
        >>= \ParseMultiDigitsResult {number, remaining, numberLength} ->
          let newValue =
                Value
                  { position = (col, row),
                    numericValue = number,
                    momentum = DirDown,
                    waiting = False
                  }
              updatedValues = push newValue vals
              updatedBlocks = blocks ++ replicate (numberLength + 1) (Util Default)
           in parseLineRecursive (col + numberLength + 1, row) updatedValues updatedBlocks remaining
parseLineRecursive (col, row) vals blocks (c : rest)
  | isDigit c =
      let newValue =
            Value
              { position = (col, row),
                numericValue = digitToInt c,
                momentum = DirDown,
                waiting = False
              }
          updatedValues = vals ++ [newValue]
          updatedBlocks = blocks ++ [Util Default]
       in parseLineRecursive (succ col, row) updatedValues updatedBlocks rest
  | otherwise =
      let assocBlock = associatedBlock c
          updatedBlocks = push assocBlock blocks
       in parseLineRecursive (succ col, row) vals updatedBlocks rest

data ParseMultiDigitsResult = ParseMultiDigitsResult
  { number :: Int,
    remaining :: String,
    numberLength :: Int
  }

parseLargeNumber :: String -> Either ParseError ParseMultiDigitsResult
parseLargeNumber ('-' : rest) =
  let positivePart = parseLargeNumber rest
   in ( \ParseMultiDigitsResult {number, remaining, numberLength} ->
          ParseMultiDigitsResult {number = -number, remaining = remaining, numberLength = succ numberLength}
      )
        <$> positivePart
parseLargeNumber str =
  let digits = takeWhile isDigit str
      isEmpty = null digits
   in if isEmpty
        then Left EmptyLiteral
        else
          Right $ parseLargeNumberRecursive 0 0 str

parseLargeNumberRecursive :: Int -> Int -> String -> ParseMultiDigitsResult
parseLargeNumberRecursive num amount (d : rest)
  | isDigit d =
      parseLargeNumberRecursive (10 * num + digitToInt d) (succ amount) rest
parseLargeNumberRecursive num amount rest =
  ParseMultiDigitsResult
    { number = num,
      remaining = rest,
      numberLength = amount
    }
