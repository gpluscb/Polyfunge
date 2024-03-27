{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Runner where

import Data.Char (chr)
import Data.Functor
import Utils

data ProgramState = ProgramState
  { cells :: [[Block]],
    values :: [Value]
  }
  deriving (Read, Show, Eq)

data Value = Value
  { position :: (Int, Int),
    numericValue :: Int,
    momentum :: Direction,
    waiting :: Bool
  }
  deriving (Read, Show, Eq)

data Direction = DirUp | DirDown | DirLeft | DirRight
  deriving (Read, Show, Eq, Enum)

isVertical :: Direction -> Bool
isVertical DirUp = True
isVertical DirDown = True
isVertical DirLeft = False
isVertical DirRight = False

isHorizontal :: Direction -> Bool
isHorizontal = not . isVertical

mirror :: Direction -> Direction
mirror DirUp = DirDown
mirror DirDown = DirUp
mirror DirLeft = DirRight
mirror DirRight = DirLeft

orthos :: Direction -> [Direction]
orthos DirUp = [DirLeft, DirRight]
orthos DirDown = orthos DirUp
orthos DirLeft = [DirUp, DirDown]
orthos DirRight = orthos DirLeft

data Block = Control ControlFlowBlock | BinaryArith BinaryArithBlock | UnaryArith UnaryArithBlock | Util UtilBlock | Io IoBlock
  deriving (Read, Show, Eq)

data ControlFlowBlock = Conveyor Direction | Wait | Jump | VGate | HGate | Question
  deriving (Read, Show, Eq)

data BinaryArithBlock = Add | Sub | Mul | Div | Mod | Gt | Lt
  deriving (Read, Show, Eq, Enum)

applyBinaryArith :: BinaryArithBlock -> Int -> Int -> Int
applyBinaryArith Add a b = a + b
applyBinaryArith Sub a b = a - b
applyBinaryArith Mul a b = a * b
applyBinaryArith Div a b = quot a b
applyBinaryArith Mod a b = mod a b
applyBinaryArith Gt a b = if a > b then 0 else 1
applyBinaryArith Lt a b = if a < b then 0 else 1

data UnaryArithBlock = Zero
  deriving (Read, Show, Eq, Enum)

applyUnaryArith :: UnaryArithBlock -> Int -> Int
applyUnaryArith Zero _ = 0

data UtilBlock = Default | Crossing | Dupe | Destroy
  deriving (Read, Show, Eq, Enum)

data IoBlock = Input | PrintDecimal | PrintAscii | Break | Halt | Error
  deriving (Read, Show, Eq, Enum)

associatedChar :: Block -> Char
associatedChar (Control (Conveyor DirUp)) = '^'
associatedChar (Control (Conveyor DirDown)) = 'v'
associatedChar (Control (Conveyor DirLeft)) = '<'
associatedChar (Control (Conveyor DirRight)) = '>'
associatedChar (Control Wait) = 'w'
associatedChar (Control Jump) = '#'
associatedChar (Control VGate) = '|'
associatedChar (Control HGate) = '_'
associatedChar (Control Question) = '?'
---------------------------
associatedChar (BinaryArith Add) = '+'
associatedChar (BinaryArith Sub) = '-'
associatedChar (BinaryArith Mul) = '*'
associatedChar (BinaryArith Div) = '/'
associatedChar (BinaryArith Mod) = '%'
associatedChar (BinaryArith Gt) = 'g'
associatedChar (BinaryArith Lt) = 'l'
---------------------------
associatedChar (UnaryArith Zero) = 'z'
---------------------------
associatedChar (Util Default) = ' '
associatedChar (Util Crossing) = '~'
associatedChar (Util Dupe) = ':'
associatedChar (Util Destroy) = 'x'
---------------------------
associatedChar (Io Input) = 'I'
associatedChar (Io PrintDecimal) = 'p'
associatedChar (Io PrintAscii) = 'P'
associatedChar (Io Break) = 'b'
associatedChar (Io Halt) = 'h'
associatedChar (Io Error) = 'e'

data EndOfProgram = Died | Halted Int | Errored Int
  deriving (Read, Show, Eq)

type IoInputOperation m = m Int

standardIoInputOperation :: IoInputOperation IO
standardIoInputOperation = getNumber

type IoOutputOperation m = String -> m ()

standardIoOutputOperation :: IoOutputOperation IO
standardIoOutputOperation = print

type IoBreakOperation m = m ()

standardIoBreakOperation :: IoBreakOperation IO
standardIoBreakOperation = void getLine

type IoOperations m = (IoInputOperation m, IoOutputOperation m, IoBreakOperation m)

standardIoOperations :: IoOperations IO
standardIoOperations = (standardIoInputOperation, standardIoOutputOperation, standardIoBreakOperation)

run :: (Monad m) => IoOperations m -> ProgramState -> m (EndOfProgram, ProgramState)
run ioOperations state =
  tick ioOperations state >>= \case
    Left end -> return (end, state)
    Right nextState -> run ioOperations nextState

tick :: (Monad m) => IoOperations m -> ProgramState -> m (Either EndOfProgram ProgramState)
tick ioOperations state =
  let blocks = cells state
      oldValues = values state
      movedValues = filter (isInBounds (gridLengths blocks)) (map moveValue oldValues)
      collisionResults =
        mapM
          ( \(x, y, block) ->
              let valuesAtBlock = filter (\Value {position} -> position == (x, y)) movedValues
               in handleCollision ioOperations block valuesAtBlock
          )
          (gridIndices blocks)
      valuesAfterCollisionHandling =
        foldl
          ( \prev next -> case (prev, next) of
              (Right xs, Right nextVals) -> Right (xs ++ nextVals)
              (Left err, _) -> Left err
              (Right _, Left err) -> Left err
          )
          (Right [])
          <$> collisionResults
      -- A board with no values may exist for one tick, therefore check old values instead of new ones
      isDead = null oldValues
   in if isDead
        then return $ Left Died
        else mapRight (\valuesAfterHandling -> state {values = valuesAfterHandling}) <$> valuesAfterCollisionHandling

moveValue :: Value -> Value
moveValue value =
  if not (waiting value)
    then
      let (x, y) = position value
       in case momentum value of
            DirUp -> value {position = (x, y + 1)}
            DirDown -> value {position = (x, y - 1)}
            DirRight -> value {position = (x + 1, y)}
            DirLeft -> value {position = (x - 1, y)}
    else value

isInBounds :: (Int, Int) -> Value -> Bool
isInBounds (boundsX, boundsY) Value {position} =
  let (x, y) = position
   in x >= 0 && y >= 0 && x < boundsX && y < boundsY

handleCollision :: (Monad m) => IoOperations m -> Block -> [Value] -> m (Either EndOfProgram [Value])
--------------------
handleCollision _ (Control (Conveyor conveyorDir)) [value] =
  return $ Right [value {momentum = conveyorDir}]
handleCollision _ (Control Wait) [value] =
  return $ Right [value {waiting = not (waiting value)}]
handleCollision _ (Control Jump) [value] =
  return $ Right [moveValue value]
handleCollision _ (Control VGate) values =
  return $
    Right $
      if any (\value -> numericValue value == 0 && isVertical (momentum value)) values
        then -- do nothing
          values
        else -- reflect
          Utils.mapIf (isHorizontal . momentum) (\value -> value {momentum = mirror (momentum value)}) values
handleCollision _ (Control HGate) values =
  return $
    Right $
      if any (\value -> numericValue value == 0 && isHorizontal (momentum value)) values
        then -- do nothing
          values
        else -- reflect
          Utils.mapIf (isVertical . momentum) (\value -> value {momentum = mirror (momentum value)}) values
handleCollision _ (Control Question) [value] =
  return $
    Right $
      if numericValue value == 0
        then -- do nothing
          [value]
        else -- copy into ortho directions
          map (\ortho -> value {momentum = ortho}) (orthos (momentum value))
-- Values annihilate each other in all control blocks (except for gates) if more than one value is present
handleCollision _ (Control _) (_ : _ : _) = return $ Right []
--------------------
handleCollision _ (BinaryArith arithBlock) [valueA, valueB] =
  let a = numericValue valueA
      b = numericValue valueB
   in return $
        Right
          [ valueA {numericValue = applyBinaryArith arithBlock a b},
            valueB {numericValue = applyBinaryArith arithBlock b a}
          ]
handleCollision _ (BinaryArith _) [value] = return $ Right [value]
-- Values annihilate each other if more than two inputs are given
handleCollision _ (BinaryArith _) (_ : _ : _) = return $ Right []
--------------------
handleCollision _ (UnaryArith arithBlock) [value] =
  return $ Right [value {numericValue = applyUnaryArith arithBlock (numericValue value)}]
-- Values annihilate each other if more than one value is present
handleCollision _ (UnaryArith _) (_ : _ : _) = return $ Right []
--------------------
handleCollision _ (Util Default) [value] = return $ Right [value]
handleCollision _ (Util Default) _ = return $ Right []
handleCollision _ (Util Crossing) values = return $ Right values
handleCollision _ (Util Dupe) [value] =
  return $ Right $ value : map (\ortho -> value {momentum = ortho}) (orthos (momentum value))
-- Values annihilate each other if more than one value is present
handleCollision _ (Util Dupe) (_ : _ : _) = return $ Right []
handleCollision _ (Util Destroy) _ = return $ Right []
--------------------
handleCollision (inputOperation, _, _) (Io Input) [value] =
  do
    c <- inputOperation
    return $ Right [value {numericValue = c}]
handleCollision (_, outputOperation, _) (Io PrintDecimal) [value] =
  do
    outputOperation $ show (numericValue value)
    return $ Right [value]
handleCollision (_, outputOperation, _) (Io PrintAscii) [value] =
  let char = chr (numericValue value)
   in do
        outputOperation $ show char
        return $ Right [value]
handleCollision (_, _, breakOperation) (Io Break) [value] = breakOperation $> Right [value]
handleCollision _ (Io Halt) [value] = return $ Left $ Halted (numericValue value)
handleCollision _ (Io Error) [value] = return $ Left $ Errored (numericValue value)
-- Values annihilate each other if more than one value is present
handleCollision _ (Io _) (_ : _ : _) = return $ Right []
--------------------
-- There are no active blocks, all blocks just manipulate incoming values
handleCollision _ _ [] = return $ Right []
