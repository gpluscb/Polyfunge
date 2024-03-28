module Parse where

import Data.Char (digitToInt, isNumber)
import ProgramData
import Utils

data ParseError = UnrecognisedChar Char | InvalidProgram
  deriving (Read, Show, Eq)

parseProgram :: String -> Either ParseError ProgramState
parseProgram program =
  let rows = lines program
      startingProgramState = ProgramState {cells = [], values = []}
   in parseProgramRecursive 0 rows startingProgramState

parseProgramRecursive :: Int -> [String] -> ProgramState -> Either ParseError ProgramState
parseProgramRecursive _ [] state = Right state
parseProgramRecursive row (line : rest) state =
  let lineParseResult = parseLine line row
      newProgramState = (\(rowValues, rowBlocks) -> state {cells = cells state ++ [rowBlocks], values = values state ++ rowValues}) <$> lineParseResult
   in parseProgramRecursive (succ row) rest =<< newProgramState

parseLine :: String -> Int -> Either ParseError ([Value], [Block])
parseLine line row = parseLineRecursive (0, row) [] [] line

parseLineRecursive :: (Int, Int) -> [Value] -> [Block] -> String -> Either ParseError ([Value], [Block])
parseLineRecursive _ vals blocks "\n" = Right (vals, blocks)
parseLineRecursive _ vals blocks "" = Right (vals, blocks)
parseLineRecursive (col, row) vals blocks (' ' : rest) =
  parseLineRecursive (succ col, row) vals (blocks ++ [Util Default]) rest
parseLineRecursive (col, row) vals blocks (c : rest)
  | isNumber c =
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
          maybeUpdatedBlocks = flip push blocks <$> assocBlock
       in case maybeUpdatedBlocks of
            Just updatedBlocks -> parseLineRecursive (succ col, row) vals updatedBlocks rest
            Nothing -> Left $ UnrecognisedChar c
