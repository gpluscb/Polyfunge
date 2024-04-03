module Utils where

import Text.Read (readMaybe)

mapIf :: (a -> Bool) -> (a -> a) -> [a] -> [a]
mapIf predicate f = map $ \x -> if predicate x then f x else x

mapEither :: (a -> c) -> (b -> d) -> Either a b -> Either c d
mapEither fl _ (Left a) = Left $ fl a
mapEither _ fr (Right b) = Right $ fr b

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f (Left a) = Left $ f a
mapLeft _ (Right b) = Right b

concatMapM :: (Monad m, Traversable t) => (a -> m [b]) -> t a -> m [b]
concatMapM f = fmap concat . mapM f

gridDimensions :: [[a]] -> (Int, Int)
gridDimensions xs =
  let numRows = length xs
      numCols = maxWidth xs
   in (numCols, numRows)

maxWidth :: [[a]] -> Int
maxWidth [] = 0
maxWidth xs = maximum $ map length xs

padToEqualWidth :: a -> [[a]] -> [[a]]
padToEqualWidth def xs = map (padTo def $ maxWidth xs) xs

padTo :: a -> Int -> [a] -> [a]
padTo d n xs = take n (xs ++ repeat d)

isInBounds :: (Int, Int) -> (Int, Int) -> Bool
isInBounds (boundsX, boundsY) position =
  let (x, y) = position
   in x >= 0 && y >= 0 && x < boundsX && y < boundsY

gridIndices :: [[a]] -> [(Int, Int, a)]
gridIndices xs =
  let rows = zip [0 ..] xs
      values = concatMap (\(rowIdx, row) -> zipWith (\colIdx x -> (colIdx, rowIdx, x)) [0 ..] row) rows
   in values

readNumber :: IO Int
readNumber = do
  line <- getLine
  maybe readNumber return (readMaybe line)

readChar :: IO Char
readChar = do
  line <- getLine
  case line of
    [c] -> return c
    _ -> readChar

push :: a -> [a] -> [a]
push = flip (++) . (: []) -- Ooooo I'm a wizard
