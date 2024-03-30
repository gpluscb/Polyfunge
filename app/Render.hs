module Render where

import Data.List (intercalate)
import ProgramData

renderTick :: Int -> ProgramState -> String
renderTick tickCount state =
  "Tick "
    ++ show tickCount
    ++ "\n\n"
    ++ renderState state
    ++ "\n"

renderState :: ProgramState -> String
renderState state =
  let blocksAndValues = blocksWithValues state
      renderedLines =
        map
          renderLine
          blocksAndValues
   in intercalate "\n" renderedLines
  where
    renderLine :: [(Block, [Value])] -> String
    renderLine =
      map
        ( \(block, valuesOnBlock) ->
            case valuesOnBlock of
              [] -> associatedChar block
              value : _ -> head $ show $ numericValue value
        )
