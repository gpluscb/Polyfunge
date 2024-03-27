module Utils where

import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.Maybe
import Text.Read (readMaybe)

mapIf :: (a -> Bool) -> (a -> a) -> [a] -> [a]
mapIf predicate f = map $ \x -> if predicate x then f x else x

gridLengths :: [[a]] -> (Int, Int)
gridLengths xs = (length $ fromMaybe [] $ listToMaybe xs, length xs)

gridIndices :: [[a]] -> [(Int, Int, a)]
gridIndices xs =
  let rows = zip [0 ..] xs
      values = concatMap (\(rowIdx, row) -> zipWith (\colIdx x -> (colIdx, rowIdx, x)) [0 ..] row) rows
   in values

mapLeft :: (a -> a') -> Either a b -> Either a' b
mapLeft f (Left x) = Left $ f x
mapLeft _ (Right y) = Right y

mapRight :: (b -> b') -> Either a b -> Either a b'
mapRight _ (Left x) = Left x
mapRight f (Right y) = Right $ f y

trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace

getNumber :: IO Int
getNumber = do
  line <- getLine
  maybe getNumber return (readMaybe (trim line))
