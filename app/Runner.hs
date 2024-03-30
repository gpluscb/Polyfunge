{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Runner where

import Control.Concurrent (threadDelay)
import Control.Monad (void)
import qualified Data.Bifunctor
import Data.Char (chr, ord)
import qualified Data.List.NonEmpty
import ProgramData
import qualified Render
import Utils

type IoInputNumberOperation m = m Int

standardIoInputOperation :: IoInputNumberOperation IO
standardIoInputOperation = do
  _ <- putStrLn "Awaiting numeric input"
  readNumber

type IoInputAsciiOperation m = m Char

standardIoInputAsciiOperation :: IoInputAsciiOperation IO
standardIoInputAsciiOperation = do
  _ <- putStrLn "Awaiting ascii input"
  readChar

type IoOutputOperation m = String -> m ()

standardIoOutputOperation :: IoOutputOperation IO
standardIoOutputOperation = putStrLn

type IoBreakOperation m = m ()

standardIoBreakOperation :: IoBreakOperation IO
standardIoBreakOperation = return ()

debugIoBreakOperation :: IoBreakOperation IO
debugIoBreakOperation = void getLine

type IoRenderOperation m = Int -> ProgramState -> m ()

standardIoRenderOperation :: IoRenderOperation IO
standardIoRenderOperation _ _ = return ()

debugIoRenderOperation :: Int -> IoRenderOperation IO
debugIoRenderOperation microsecs tickCount state = do
  _ <- threadDelay microsecs
  putStrLn $ Render.renderTick tickCount state

data IoOperations m = IoOperations
  { inputNumber :: IoInputNumberOperation m,
    inputAscii :: IoInputAsciiOperation m,
    output :: IoOutputOperation m,
    breakOp :: IoBreakOperation m,
    renderOp :: IoRenderOperation m
  }

standardIoOperations :: IoOperations IO
standardIoOperations =
  IoOperations
    { inputNumber = standardIoInputOperation,
      inputAscii = standardIoInputAsciiOperation,
      output = standardIoOutputOperation,
      breakOp = standardIoBreakOperation,
      renderOp = standardIoRenderOperation
    }

debugIoOperations :: Int -> IoOperations IO
debugIoOperations microsecs =
  IoOperations
    { inputNumber = standardIoInputOperation,
      inputAscii = standardIoInputAsciiOperation,
      output = standardIoOutputOperation,
      breakOp = debugIoBreakOperation,
      renderOp = debugIoRenderOperation microsecs
    }

runNormal :: ProgramState -> IO EndOfProgram
runNormal = fmap fst . run standardIoOperations

runDebug :: Int -> ProgramState -> IO EndOfProgram
runDebug microsecs = fmap fst . run (debugIoOperations microsecs)

run :: (Monad m) => IoOperations m -> ProgramState -> m (EndOfProgram, ProgramState)
run = runRecursive 0

runRecursive :: (Monad m) => Int -> IoOperations m -> ProgramState -> m (EndOfProgram, ProgramState)
runRecursive tickCount ioOperations state =
  do
    _ <- renderOp ioOperations tickCount state
    tick ioOperations state >>= \case
      Left end -> return (end, state)
      Right nextState -> runRecursive (succ tickCount) ioOperations nextState

tick :: (Monad m) => IoOperations m -> ProgramState -> m (Either EndOfProgram ProgramState)
tick ioOperations state =
  let blocks = cells state
      oldValues = values state
      -- First advance values leaving jump pad by one
      jumpedValues =
        mapIf
          ( \Value {position = (x, y)} ->
              blocks !! y !! x == Control Jump
          )
          moveValue
          oldValues
      -- Move values and delete out of bounds ones
      movedValues = filter (isInBounds (gridDimensions blocks) . position) (map moveValue jumpedValues)
      -- Find values at each block
      blocksWithMovedValues = concat $ blocksWithValues state {values = movedValues}
      -- Do fusion
      blocksWithValuesGroupedByMomentum =
        map (Data.Bifunctor.second (Data.List.NonEmpty.groupAllWith momentum)) blocksWithMovedValues
      blocksWithValuesAfterFusion =
        map (Data.Bifunctor.second doFusion) blocksWithValuesGroupedByMomentum
        where
          doFusion =
            map $
              \values ->
                let updatedNumericValue = sum $ Data.List.NonEmpty.map numericValue values
                    updatedWaiting = any waiting values
                 in (Data.List.NonEmpty.head values) {numericValue = updatedNumericValue, waiting = updatedWaiting}

      -- Handle collisions
      collisionResults = mapM (uncurry (handleCollision ioOperations)) blocksWithValuesAfterFusion
      valuesAfterCollisionHandling =
        foldl
          ( \a b ->
              case (a, b) of
                -- Errors have highest priority
                (e@(Left (Errored _)), _) -> e
                (_, e@(Left (Errored _))) -> e
                -- Next priority is halted
                (h@(Left (Halted _)), _) -> h
                (_, h@(Left (Halted _))) -> h
                -- Next priority is any other termination
                -- Dath can't be returned here but still
                (t@(Left _), _) -> t
                (_, t@(Left _)) -> t
                -- Lastly, we know both are okay
                (Right valuesA, Right valuesB) -> Right $ valuesA ++ valuesB
          )
          (Right [])
          <$> collisionResults
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
            DirUp -> value {position = (x, pred y)}
            DirDown -> value {position = (x, succ y)}
            DirRight -> value {position = (succ x, y)}
            DirLeft -> value {position = (pred x, y)}
    else value

handleCollision :: (Monad m) => IoOperations m -> Block -> [Value] -> m (Either EndOfProgram [Value])
--------------------
handleCollision _ (Control (Conveyor conveyorDir)) [value] =
  return $ Right [value {momentum = conveyorDir}]
handleCollision _ (Control Wait) [value] =
  return $ Right [value {waiting = not (waiting value)}]
-- Stepping onto the jump block does not do anything, only leaving it does
handleCollision _ (Control Jump) [value] =
  return $ Right [value]
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
handleCollision _ (Control Test) [value] =
  -- Keep 0, discard otherwise
  return $ Right $ filter ((== 0) . numericValue) [value]
handleCollision _ (Control Not) [value] =
  -- Discard 0, keep otherwise
  return $ Right $ filter ((/= 0) . numericValue) [value]
-- Values annihilate each other in all control blocks (except for gates) if more than one value is present
handleCollision _ (Control _) (_ : _ : _) = return $ Right []
--------------------
handleCollision _ (BinaryArith arithBlock) [valueA, valueB] =
  let a = numericValue valueA
      b = numericValue valueB
      maybeResultA = applyBinaryArith arithBlock a b
      maybeResultB = applyBinaryArith arithBlock b a
      results = case (maybeResultA, maybeResultB) of
        (Just resA, Just resB) -> Just (resA, resB)
        (_, _) -> Nothing
   in return $
        Right $
          maybe
            [] -- Annihilation on invalid computation (e.g. division by 0)
            ( \(resA, resB) ->
                [ valueA {numericValue = resA},
                  valueB {numericValue = resB}
                ]
            )
            results
handleCollision _ (BinaryArith _) [value] = return $ Right [value]
-- Values annihilate each other if more than two inputs are given
handleCollision _ (BinaryArith _) (_ : _ : _ : _) = return $ Right []
--------------------
handleCollision _ (UnaryArith arithBlock) [value] =
  return $ Right [value {numericValue = applyUnaryArith arithBlock (numericValue value)}]
-- Values annihilate each other if more than one value is present
handleCollision _ (UnaryArith _) (_ : _ : _) = return $ Right []
--------------------
handleCollision _ (Util Default) [value] = return $ Right [value]
handleCollision _ (Util Dupe) [value] =
  return $ Right $ value : map (\ortho -> value {momentum = ortho}) (orthos (momentum value))
handleCollision _ (Util Destroy) _ = return $ Right []
-- Values annihilate each other if more than one value is present
handleCollision _ (Util _) (_ : _ : _) = return $ Right []
--------------------
handleCollision IoOperations {inputNumber} (Io InputDecimal) [value] =
  do
    n <- inputNumber
    return $ Right [value {numericValue = n}]
handleCollision IoOperations {inputAscii} (Io InputAscii) [value] =
  do
    c <- inputAscii
    return $ Right [value {numericValue = ord c}]
handleCollision IoOperations {output} (Io PrintDecimal) [value] =
  do
    output $ show (numericValue value)
    return $ Right [value]
handleCollision IoOperations {output} (Io PrintAscii) [value] =
  let char = chr (numericValue value)
   in do
        output [char]
        return $ Right [value]
handleCollision IoOperations {breakOp} (Io Break) [value] = do
  _ <- breakOp
  return $ Right [value]
handleCollision _ (Io Halt) [value] = return $ Left $ Halted (numericValue value)
handleCollision _ (Io Error) [value] = return $ Left $ Errored (numericValue value)
-- Values annihilate each other if more than one value is present
handleCollision _ (Io _) (_ : _ : _) = return $ Right []
--------------------
handleCollision io (Unrecognised _) values = handleCollision io (Util Default) values
--------------------
-- Default case: there are no active blocks, all blocks just manipulate incoming values
handleCollision _ _ [] = return $ Right []
