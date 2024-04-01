{-# LANGUAGE NumericUnderscores #-}

module Main where

import Data.Maybe (fromMaybe)
import qualified Parse
import ProgramData (EndOfProgram (Aborted, Died, Errored, Halted))
import qualified Runner
import System.Environment (getArgs)
import qualified System.Exit

main :: IO ()
main = mainWithArgs =<< getArgs

mainWithArgs :: [String] -> IO ()
mainWithArgs [path] = System.Exit.exitWith =<< execute path Nothing
mainWithArgs ["--debug", path] = System.Exit.exitWith =<< execute path (Just 50_000)
mainWithArgs _ = System.Exit.die "Usage: exec [--debug] <program file>"

execute :: FilePath -> Maybe Int -> IO System.Exit.ExitCode
execute path debugMicros = do
  program <- readFile path
  result <-
    sequence $
      (fromMaybe Runner.runNormal (Runner.runDebug <$> debugMicros))
        <$> (Parse.parseProgram program)
  case result of
    Left Parse.EmptyLiteral -> do
      _ <- putStrLn "Parse error: Empty char or number literal"
      return $ System.Exit.ExitFailure $ -1
    Right Aborted -> do
      _ <- putStrLn "Program aborted"
      return $ System.Exit.ExitFailure $ -2
    Right Died -> do
      _ <- putStrLn "Program died without output"
      return System.Exit.ExitSuccess
    Right (Halted out) -> do
      _ <- putStrLn $ "Output: " ++ show out
      return System.Exit.ExitSuccess
    Right (Errored e) -> do
      _ <- putStrLn $ "Error output: " ++ show e
      return $ System.Exit.ExitFailure e
