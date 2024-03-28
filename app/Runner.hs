{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Runner where

import Data.Char (chr)
import Data.Functor
import ProgramData
import Utils

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
      movedValues = filter (isInBounds (gridDimensions blocks) . position) (map moveValue oldValues)
      collisionResults =
        mapM
          ( \(x, y, block) ->
              let valuesAtBlock = filter (\Value {position} -> position == (x, y)) movedValues
               in handleCollision ioOperations block valuesAtBlock
          )
          (gridIndices blocks)
      valuesAfterCollisionHandling = (fmap . fmap) concat (sequence <$> collisionResults)
      -- A board with no values may exist for one tick, therefore check old values instead of new ones
      isDead = null oldValues
   in if isDead
        then return $ Left Died
        else fmap (\valuesAfterHandling -> state {values = valuesAfterHandling}) <$> valuesAfterCollisionHandling

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
