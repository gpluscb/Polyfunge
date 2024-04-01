{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Control.Monad.Trans.State
import Data.Char (digitToInt)
import Parse (ParseMultiDigitsResult (number), parseLargeNumber, parseProgram)
import ProgramData (EndOfProgram (Died, Halted), ProgramState)
import Runner (ContinueAction (Continue), IoOperations (IoOperations, debugger, inputAscii, inputNumber, output, render), run)
import qualified System.Exit as Exit
import Test.HUnit (Counts (failures), Test (TestCase, TestLabel, TestList), assertFailure, runTestTT)
import TestUtils (readFilesInDirRecursive)
import Utils (mapEither, mapLeft)

main :: IO ()
main = do
  tests <- testsForDirectory "./testcases/"
  result <- runTestTT tests
  if failures result > 0
    then Exit.exitFailure
    else Exit.exitSuccess

testsForDirectory :: FilePath -> IO Test
testsForDirectory dirPath = do
  ranTests <- runTestCases dirPath
  return $ TestList $ map (\(path, result) -> TestLabel path $ TestCase (eitherAssertion result)) ranTests

eitherAssertion :: (Show a) => Either a b -> IO ()
eitherAssertion (Left e) = assertFailure $ show e
eitherAssertion _ = return ()

data TestError
  = Parse String
  | EndOfProgramMismatch {actualEop :: EndOfProgram, expectedEop :: EndOfProgram}
  | MoreInputsThanExpected
  | FewerInputsThanExpected {remaining :: [IoElement]}
  | NumInputWhenCharExpected
  | CharInputWhenNumExpected
  | OutputMismatch {actualOut :: [String], expectedOut :: [String]}
  deriving (Read, Show, Eq)

data IoElement = Int Int | Char Char
  deriving (Read, Show, Eq)

data IoState = IoState
  { remainingInput :: [IoElement],
    collectedOutput :: [String]
  }
  deriving (Read, Show, Eq)

data TestFile = TestFile
  { inputBuffer :: [IoElement],
    expectedOutput :: [String],
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
  let result = run Continue testIoOperations program
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

    checkCollectedOutput :: [String] -> [String] -> Either TestError ()
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
parseInputBufferLine _ = Left "Test file: input buffer line is invalid"

parseIoElement :: String -> Either String IoElement
parseIoElement [n] = Right $ Int $ digitToInt n
parseIoElement ('(' : n) =
  mapEither (\_ -> "Test file: Input buffer has invalid number literal") (Int . number) $ parseLargeNumber n
parseIoElement ['\'', c] = Right $ Char c
parseIoElement _ = Left "Test file: Input buffer must contain valid polyfunge literals"

parseExpectedOutputLine :: String -> Either String [String]
parseExpectedOutputLine ('O' : ' ' : rest) = Right $ words rest
parseExpectedOutputLine _ = Left "Test file: expected output line is invalid"

parseExpectedEopLine :: String -> Either String EndOfProgram
parseExpectedEopLine "D" = Right Died
parseExpectedEopLine ('H' : ' ' : rest) =
  mapEither (\_ -> "Test file: expected halting with not a number") (Halted . number) $ parseLargeNumber rest
parseExpectedEopLine ('E' : ' ' : rest) =
  mapEither (\_ -> "Test file: expected error with not a number") (Halted . number) $ parseLargeNumber rest
parseExpectedEopLine _ = Left "Test file: expected eop line is invalid"

parseFenceLine :: String -> Either String ()
parseFenceLine ('-' : '-' : '-' : _) = Right ()
parseFenceLine _ = Left "Test file: fence line is invalid"

type StateTransformer i o = o -> i -> IoState -> (o, Either TestError IoState) -- Only used in where clause in testIoOperations

testIoOperations :: IoOperations (State (Either TestError IoState))
testIoOperations =
  IoOperations
    { inputNumber = state $ tryApplyTransformer inputNumberTransformer 0 (),
      inputAscii = state $ tryApplyTransformer inputAsciiTransformer ' ' (),
      output = state . tryApplyTransformer outputTransformer (),
      debugger = state $ tryApplyTransformer returnDefaultTransformer Continue (),
      render = curry $ state . tryApplyTransformer returnDefaultTransformer ()
    }
  where
    tryApplyTransformer :: StateTransformer i o -> o -> i -> Either TestError IoState -> (o, Either TestError IoState)
    tryApplyTransformer transformer default' input maybeTransformerInput = case maybeTransformerInput of
      e@(Left _) -> (default', e)
      Right prevState -> transformer default' input prevState

    inputNumberTransformer :: StateTransformer () Int
    inputNumberTransformer default' () IoState {remainingInput = []} = (default', Left MoreInputsThanExpected)
    inputNumberTransformer default' () IoState {remainingInput = (Char _) : _} = (default', Left CharInputWhenNumExpected)
    inputNumberTransformer _ () prevState@IoState {remainingInput = (Int n) : rest} =
      ( n,
        Right prevState {remainingInput = rest}
      )

    inputAsciiTransformer :: StateTransformer () Char
    inputAsciiTransformer default' () IoState {remainingInput = []} = (default', Left MoreInputsThanExpected)
    inputAsciiTransformer default' () IoState {remainingInput = (Int _) : _} = (default', Left NumInputWhenCharExpected)
    inputAsciiTransformer _ () prevState@IoState {remainingInput = (Char c) : rest} =
      ( c,
        Right prevState {remainingInput = rest}
      )

    outputTransformer :: StateTransformer String ()
    outputTransformer () input prevState@IoState {collectedOutput} =
      ( (),
        Right prevState {collectedOutput = collectedOutput ++ [input]}
      )

    returnDefaultTransformer :: StateTransformer i o
    returnDefaultTransformer default' _ prevState =
      ( default',
        Right prevState
      )
