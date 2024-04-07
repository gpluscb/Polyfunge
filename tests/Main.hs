{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Control.Monad.Trans.State (State, runState, state)
import Data.Char (chr, digitToInt, isNumber)
import Parse (ParseMultiDigitsResult (number), parseLargeNumber, parseProgram)
import ProgramData (Direction (DirDown, DirLeft, DirRight, DirUp), EndOfProgram (Died, Errored, Halted), ProgramState, TickInfo)
import Runner (ContinueAction (Continue), CustomOperations (CustomOperations, inputAscii, inputNumber, inputRandom, inspectTick, outputAscii, outputNumber), run)
import Test.HUnit (Test (TestCase, TestLabel, TestList), runTestTTAndExit)
import TestUtils
import Utils (mapEither, mapLeft)

main :: IO ()
main = do
  tests <- testsForDirectory "./testcases/"
  runTestTTAndExit tests

testsForDirectory :: FilePath -> IO Test
testsForDirectory dirPath = do
  ranTests <- runTestCases dirPath
  return $ TestList $ map (\(path, result) -> TestLabel path $ TestCase (eitherAssertion result)) ranTests

data TestError
  = Parse String
  | EndOfProgramMismatch {actualEop :: EndOfProgram, expectedEop :: EndOfProgram}
  | MoreInputsThanExpected
  | FewerInputsThanExpected {remaining :: [IoElement]}
  | DifferentInputTypeThanExpected
  | OutputMismatch {actualOut :: [IoElement], expectedOut :: [IoElement]}
  deriving (Read, Show, Eq)

data IoElement = Int Int | Char Char | Direction Direction
  deriving (Read, Show, Eq)

data IoState = IoState
  { remainingInput :: [IoElement],
    collectedOutput :: [IoElement]
  }
  deriving (Read, Show, Eq)

data TestFile = TestFile
  { inputBuffer :: [IoElement],
    expectedOutput :: [IoElement],
    expectedExit :: EndOfProgram,
    program :: ProgramState
  }
  deriving (Read, Show, Eq)

runTestCases :: FilePath -> IO [(FilePath, Either TestError ())]
runTestCases dirPath = do
  testCases <- getTestCases dirPath
  let runResults = map (\(path, testFileResult) -> (path, runTestFile =<< mapLeft Parse testFileResult)) testCases
   in return runResults

runTestFile :: TestFile -> Either TestError ()
runTestFile TestFile {inputBuffer, expectedOutput, expectedExit, program} =
  let result = run program testOperations
      initialIoState = IoState {remainingInput = inputBuffer, collectedOutput = []}
      ((endOfProgram, _), stateResult) = runState result $ Right initialIoState
   in do
        IoState {remainingInput, collectedOutput} <- stateResult
        _ <- checkRemainingInput remainingInput
        _ <- checkCollectedOutput collectedOutput expectedOutput
        _ <- checkExit endOfProgram expectedExit
        Right ()
  where
    checkRemainingInput :: [IoElement] -> Either TestError ()
    checkRemainingInput [] = Right ()
    checkRemainingInput remaining = Left FewerInputsThanExpected {remaining}

    checkCollectedOutput :: [IoElement] -> [IoElement] -> Either TestError ()
    checkCollectedOutput actualOut expectedOut =
      errorOnMismatch OutputMismatch {actualOut, expectedOut} actualOut expectedOut

    checkExit :: EndOfProgram -> EndOfProgram -> Either TestError ()
    checkExit actualEop expectedEop =
      errorOnMismatch (EndOfProgramMismatch {actualEop, expectedEop}) actualEop expectedEop

    errorOnMismatch :: (Eq a) => TestError -> a -> a -> Either TestError ()
    errorOnMismatch e a b
      | a == b = Right ()
      | otherwise = Left e

getTestCases :: FilePath -> IO [(FilePath, Either String TestFile)]
getTestCases dirPath = do
  fileContents <- readFilesInDirRecursive dirPath
  return $
    map
      (\(path, contents) -> (path, parseTestFile contents))
      fileContents

parseTestFile :: String -> Either String TestFile
parseTestFile str =
  let strLines = lines str
      (headerLines, programLines) = splitAt 4 strLines
      programStr = unlines programLines
      parseProgramResult = mapLeft (("Test file: Program parse error: " ++) . show) $ parseProgram programStr
      testFile = parseTestFileHeader headerLines =<< parseProgramResult
   in testFile

parseTestFileHeader :: [String] -> ProgramState -> Either String TestFile
parseTestFileHeader [inputBufferLine, expectedOutputLine, expectedEopLine, fenceLine] program = do
  inputBuffer <- parseInputBufferLine inputBufferLine
  expectedOutput <- parseExpectedOutputLine expectedOutputLine
  expectedEop <- parseExpectedEopLine expectedEopLine
  _ <- parseFenceLine fenceLine
  return
    TestFile
      { inputBuffer,
        expectedOutput,
        expectedExit = expectedEop,
        program
      }
parseTestFileHeader _ _ = Left "Test file: Not enough header lines"

parseInputBufferLine :: String -> Either String [IoElement]
parseInputBufferLine ('I' : ' ' : rest) = sequence $ map parseIoElement $ words rest
parseInputBufferLine "I" = Right []
parseInputBufferLine _ = Left "Test file: input buffer line is invalid"

parseIoElement :: String -> Either String IoElement
parseIoElement [n] | isNumber n = Right $ Int $ digitToInt n
parseIoElement ('(' : n) =
  mapEither (\_ -> "Test file: Invalid number literal") (Int . number) $ parseLargeNumber n
parseIoElement ['\'', c] = Right $ Char c
parseIoElement ['^'] = Right $ Direction DirUp
parseIoElement ['v'] = Right $ Direction DirDown
parseIoElement ['<'] = Right $ Direction DirLeft
parseIoElement ['>'] = Right $ Direction DirRight
parseIoElement _ =
  Left
    "Test file: Input and output buffer must contain valid polyfunge literals. \
    \Input buffer may also have conveyor directions"

parseExpectedOutputLine :: String -> Either String [IoElement]
parseExpectedOutputLine ('O' : ' ' : rest) = sequence $ map parseIoElement $ words rest
parseExpectedOutputLine "O" = Right []
parseExpectedOutputLine _ = Left "Test file: expected output line is invalid"

parseExpectedEopLine :: String -> Either String EndOfProgram
parseExpectedEopLine "D" = Right Died
parseExpectedEopLine ('H' : ' ' : rest) =
  mapEither (\_ -> "Test file: expected halting with not a number") (Halted . number) $ parseLargeNumber rest
parseExpectedEopLine ('E' : ' ' : rest) =
  mapEither (\_ -> "Test file: expected error with not a number") (Errored . number) $ parseLargeNumber rest
parseExpectedEopLine _ = Left "Test file: expected eop line is invalid"

parseFenceLine :: String -> Either String ()
parseFenceLine ('-' : '-' : '-' : _) = Right ()
parseFenceLine _ = Left "Test file: fence line is invalid"

type StateTransformer i o = o -> i -> IoState -> (o, Either TestError IoState) -- Only used in where clause in testIoOperations

testOperations :: CustomOperations (State (Either TestError IoState))
testOperations =
  CustomOperations
    { inputNumber = state $ tryApplyTransformer inputNumberTransformer 0 (),
      inputAscii = state $ tryApplyTransformer inputAsciiTransformer ' ' (),
      inputRandom = state $ tryApplyTransformer inputRandomTransformer DirDown (),
      outputNumber = state . tryApplyTransformer outputNumberTransformer (),
      outputAscii = state . tryApplyTransformer outputAsciiTransformer (),
      inspectTick = state . tryApplyTransformer inspectTickTransformer Continue
    }
  where
    tryApplyTransformer :: StateTransformer i o -> o -> i -> Either TestError IoState -> (o, Either TestError IoState)
    tryApplyTransformer transformer default' input maybeTransformerInput = case maybeTransformerInput of
      e@(Left _) -> (default', e)
      Right prevState -> transformer default' input prevState

    inputNumberTransformer :: StateTransformer () Int
    inputNumberTransformer default' () IoState {remainingInput = []} = (default', Left MoreInputsThanExpected)
    inputNumberTransformer _ () prevState@IoState {remainingInput = (Int n) : rest} =
      ( n,
        Right prevState {remainingInput = rest}
      )
    inputNumberTransformer default' () IoState {remainingInput = _ : _} = (default', Left DifferentInputTypeThanExpected)

    inputAsciiTransformer :: StateTransformer () Char
    inputAsciiTransformer default' () IoState {remainingInput = []} = (default', Left MoreInputsThanExpected)
    inputAsciiTransformer _ () prevState@IoState {remainingInput = (Char c) : rest} =
      ( c,
        Right prevState {remainingInput = rest}
      )
    inputAsciiTransformer default' () IoState {remainingInput = _ : _} = (default', Left DifferentInputTypeThanExpected)

    inputRandomTransformer :: StateTransformer () Direction
    inputRandomTransformer default' () IoState {remainingInput = []} = (default', Left MoreInputsThanExpected)
    inputRandomTransformer _ () prevState@IoState {remainingInput = (Direction d) : rest} =
      ( d,
        Right prevState {remainingInput = rest}
      )
    inputRandomTransformer default' () IoState {remainingInput = _ : _} = (default', Left DifferentInputTypeThanExpected)

    outputTransformer :: StateTransformer IoElement ()
    outputTransformer () input prevState@IoState {collectedOutput} =
      ( (),
        Right prevState {collectedOutput = collectedOutput ++ [input]}
      )

    outputNumberTransformer :: StateTransformer Int ()
    outputNumberTransformer () input = outputTransformer () (Int input)

    outputAsciiTransformer :: StateTransformer Int ()
    outputAsciiTransformer () input = outputTransformer () (Char $ chr input)

    inspectTickTransformer :: StateTransformer TickInfo ContinueAction
    inspectTickTransformer default' _ prevState =
      ( default',
        Right prevState
      )
