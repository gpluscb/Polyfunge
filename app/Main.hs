module Main where

import qualified Parse
import qualified Runner

main :: IO ()
main =
  do
    x <-
      mapM
        (Runner.run Runner.standardIoOperations)
        ( Parse.parseProgram helloWorldExample
        )
    print x

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

infiniteLoopExample :: String
infiniteLoopExample =
  "1 \n\
  \><"