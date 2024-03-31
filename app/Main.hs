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

fibExample :: String
fibExample =
  "1     \n\
  \w     \n\
  \vx# #<\n\
  \::zv  \n\
  \pw    \n\
  \ w   #\n\
  \ 0x   \n\
  \ >:< #\n\
  \> +  ^"

helloWorldExample :: String
helloWorldExample =
  "'!'d'l'r'o'w' ','o'l'l'e'H \n\
  \> > > > > > > > > > > > > P"

cuteExample :: String
cuteExample =
  "   0   (-3 (-82(67 (18 (-1 (-15(-101\n\
  \(117\n\
  \ xv<   <   <   <   <   <   <   <\n\
  \>:+ <\n\
  \  P\n\
  \x?x\n\
  \ h"

primesExample :: String
primesExample =
  "               1          2           \n\
  \             1x         2x            \n\
  \             >:<        >:<           \n\
  \     2      > +         >+        v   \n\
  \   2x         x          x            \n\
  \   >:<    5      > # # # # # v        \n\
  \>   +     v           > _x            \n\
  \    x    x:z^  v  #w <z               \n\
  \          v    # #  x?^ w           x \n\
  \^  p   < x:      :   gx      >    % ?v\n\
  \      x  x_www < x      #         w x \n\
  \^ ww  |^# <          #            w   \n\
  \               #  x >*x       x   w   \n\
  \      ^   w w w: ww   # #     :  # # <\n\
  \3              x    #w            w   \n\
  \                    ^:x  x        w   \n\
  \2                    ^   : < x|v# <   \n\
  \                           ^   <      \n\
  \p                             x       \n\
  \x                       #             \n\
  \                        ^<            "

erorrPriorityExample :: String
erorrPriorityExample =
  "1234\n\
  \pIhe"

infiniteLoopExample :: String
infiniteLoopExample =
  "1 \n\
  \><"
