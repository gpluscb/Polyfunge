module Render where

import Data.List (intercalate)
import Data.Maybe (fromMaybe, listToMaybe)
import ProgramData

renderTick :: Int -> String -> ProgramState -> String
renderTick tickCount output state =
  "Tick "
    ++ show tickCount
    ++ "\n\n"
    ++ renderState state
    ++ "\n"
    ++ "Program output: "
    ++ output
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
      map $ \(block, valuesOnBlock) ->
        case valuesOnBlock of
          [] -> associatedChar block
          value : _ -> fromMaybe '0' $ listToMaybe $ show $ numericValue value
