module Main where

import qualified Parse
import qualified Runner

main :: IO ()
main =
  do
    x <-
      mapM
        (Runner.run Runner.standardIoOperations)
        ( Parse.parseProgram primesExample
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

infiniteLoopExample :: String
infiniteLoopExample =
  "1 \n\
  \><"
