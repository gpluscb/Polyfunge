{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Runner where

import Control.Concurrent (threadDelay)
import qualified Data.Bifunctor
import Data.Char (chr, ord)
import qualified Data.List.NonEmpty
import ProgramData
import qualified Render
import System.Random (randomRIO)
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

type IoRandomInputOperation m = m Direction

standardIoRandomInputOperation :: IoRandomInputOperation IO
standardIoRandomInputOperation = do
  num <- randomRIO (0, 3) :: IO Int
  return $ case num of
    0 -> DirUp
    1 -> DirDown
    2 -> DirLeft
    3 -> DirRight
    _ -> error "Unreachable"

type IoOutputOperation m = String -> m ()

standardIoOutputOperation :: IoOutputOperation IO
standardIoOutputOperation = putStrLn

type IoDebuggerOperation m = m ContinueAction

data ContinueAction = Continue | Step | Abort

standardIoDebuggerOperation :: IoDebuggerOperation IO
standardIoDebuggerOperation = return Continue

debugIoDebuggerOperation :: IoDebuggerOperation IO
debugIoDebuggerOperation = do
  _ <- putStrLn "Program paused. Press enter or s to step once, c to continue, or a to abort"
  getDebuggerAction
  where
    getDebuggerAction =
      do
        action <- getLine
        case action of
          "c" -> return Continue
          "a" -> return Abort
          c | c `elem` ["s", ""] -> return Step
          _ -> getDebuggerAction

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
    inputRandom :: IoRandomInputOperation m,
    output :: IoOutputOperation m,
    debugger :: IoDebuggerOperation m,
    render :: IoRenderOperation m
  }

standardIoOperations :: IoOperations IO
standardIoOperations =
  IoOperations
    { inputNumber = standardIoInputOperation,
      inputAscii = standardIoInputAsciiOperation,
      inputRandom = standardIoRandomInputOperation,
      output = standardIoOutputOperation,
      debugger = standardIoDebuggerOperation,
      render = standardIoRenderOperation
    }

debugIoOperations :: Int -> IoOperations IO
debugIoOperations microsecs =
  IoOperations
    { inputNumber = standardIoInputOperation,
      inputAscii = standardIoInputAsciiOperation,
      inputRandom = standardIoRandomInputOperation,
      output = standardIoOutputOperation,
      debugger = debugIoDebuggerOperation,
      render = debugIoRenderOperation microsecs
    }

runNormal :: ProgramState -> IO EndOfProgram
runNormal = fmap fst . run Continue standardIoOperations

runDebug :: Int -> ProgramState -> IO EndOfProgram
runDebug microsecs = fmap fst . run Step (debugIoOperations microsecs)

run :: (Monad m) => ContinueAction -> IoOperations m -> ProgramState -> m (EndOfProgram, ProgramState)
run = runRecursive 0

runRecursive :: (Monad m) => Int -> ContinueAction -> IoOperations m -> ProgramState -> m (EndOfProgram, ProgramState)
runRecursive tickCount action ioOperations state =
  do
    _ <- render ioOperations tickCount state
    case action of
      Continue -> do
        result <- tick False ioOperations state
        case result of
          Left end -> return (end, state)
          Right (nextState, nextAction) -> runRecursive (succ tickCount) nextAction ioOperations nextState
      Step -> do
        nextAction <- debugger ioOperations
        result <- tick True ioOperations state
        case result of
          Left end -> return (end, state)
          Right (nextState, _) -> runRecursive (succ tickCount) nextAction ioOperations nextState
      Abort -> return $ (Aborted, state)

tick :: (Monad m) => Bool -> IoOperations m -> ProgramState -> m (Either EndOfProgram (ProgramState, ContinueAction))
tick stepping ioOperations state =
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
                    updatedWaiting = all waiting values -- If one has not already waited, result should wait again
                 in (Data.List.NonEmpty.head values) {numericValue = updatedNumericValue, waiting = updatedWaiting}
      -- Handle collisions
      collisionResults = mapM (uncurry (handleCollision stepping ioOperations)) blocksWithValuesAfterFusion
      valuesAfterCollisionHandling =
        foldl
          ( \a b ->
              let extractPriorityAction actionA actionB = case (actionA, actionB) of
                    -- Abort has highest priority
                    (Abort, _) -> Abort
                    (_, Abort) -> Abort
                    -- Next priority is Step
                    (Step, _) -> Step
                    (_, Step) -> Step
                    -- Lastly, we know it's continue
                    (Continue, Continue) -> Continue
                  extractPriorityCollisionResult resultA resultB = case (resultA, resultB) of
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
                    (Right (valuesA, actionA), Right (valuesB, actionB)) ->
                      Right $
                        ( valuesA ++ valuesB,
                          extractPriorityAction actionA actionB
                        )
               in extractPriorityCollisionResult a b
          )
          (Right ([], Continue))
          <$> collisionResults
      -- A board with no values may exist for one tick, therefore check old values instead of new ones
      isDead = null oldValues
   in if isDead
        then return $ Left Died
        else fmap (\(valuesAfterHandling, action) -> (state {values = valuesAfterHandling}, action)) <$> valuesAfterCollisionHandling

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

handleCollision :: (Monad m) => Bool -> IoOperations m -> Block -> [Value] -> m (Either EndOfProgram ([Value], ContinueAction))
--------------------
handleCollision _ _ (Control (Conveyor conveyorDir)) [value] =
  return $ Right ([value {momentum = conveyorDir}], Continue)
handleCollision _ IoOperations {inputRandom} (Control Spinner) [value] = do
  newMomentum <- inputRandom
  return $ Right ([value {momentum = newMomentum}], Continue)
handleCollision _ _ (Control Wait) [value] =
  return $ Right ([value {waiting = not (waiting value)}], Continue)
-- Stepping onto the jump block does not do anything, only leaving it does
handleCollision _ _ (Control Jump) [value] =
  return $ Right ([value], Continue)
handleCollision _ _ (Control (Gate gateOrientation)) [value] =
  return $
    Right $
      ( if orientationFromDirection (momentum value) == gateOrientation
          then [value] -- Values in the direction of the gate get to pass
          else [value {momentum = mirror (momentum value)}], -- Others reflect
        Continue
      )
-- If two values align with the gate, two values do not align with the gate, or three or more values are present
-- they will be annihilated in default case
handleCollision x y z@(Control (Gate gateOrientation)) [valueA, valueB]
  | orientationFromDirection (momentum valueB) == gateOrientation
      && orientationFromDirection (momentum valueA) /= gateOrientation =
      handleCollision x y z [valueB, valueA] -- Reorder such that first value aligns with gate
  | orientationFromDirection (momentum valueA) == gateOrientation
      && orientationFromDirection (momentum valueB) /= gateOrientation =
      return $
        Right $
          ( if numericValue valueA == 0
              then [valueA, valueB] -- 0 allows passing of both
              else [valueA, valueB {momentum = mirror (momentum valueB)}], -- Everything else mirrors
            Continue
          )
handleCollision _ _ (Control Test) [value] =
  -- Keep 0, discard otherwise
  return $ Right (filter ((== 0) . numericValue) [value], Continue)
handleCollision _ _ (Control Not) [value] =
  -- Discard 0, keep otherwise
  return $ Right (filter ((/= 0) . numericValue) [value], Continue)
-- Values annihilate each other in all control blocks (except for gates) if more than one value is present
handleCollision _ _ (Control _) (_ : _ : _) = return $ Right ([], Continue)
--------------------
handleCollision _ _ (BinaryArith arithBlock) [valueA, valueB] =
  let a = numericValue valueA
      b = numericValue valueB
      maybeResultA = applyBinaryArith arithBlock a b
      maybeResultB = applyBinaryArith arithBlock b a
      results = case (maybeResultA, maybeResultB) of
        (Just resA, Just resB) -> Just (resA, resB)
        (_, _) -> Nothing
   in return $
        Right
          ( maybe
              [] -- Annihilation on invalid computation (e.g. division by 0)
              ( \(resA, resB) ->
                  [ valueA {numericValue = resA},
                    valueB {numericValue = resB}
                  ]
              )
              results,
            Continue
          )
handleCollision _ _ (BinaryArith _) [value] = return $ Right ([value], Continue)
-- Values annihilate each other if more than two inputs are given
handleCollision _ _ (BinaryArith _) (_ : _ : _ : _) = return $ Right ([], Continue)
--------------------
handleCollision _ _ (UnaryArith arithBlock) [value] =
  return $
    Right
      ( [value {numericValue = applyUnaryArith arithBlock (numericValue value)}],
        Continue
      )
-- Values annihilate each other if more than one value is present
handleCollision _ _ (UnaryArith _) (_ : _ : _) = return $ Right ([], Continue)
--------------------
handleCollision _ _ (Util Default) [value] = return $ Right ([value], Continue)
handleCollision _ _ (Util Dupe) [value] =
  return $
    Right
      ( value : map (\ortho -> value {momentum = ortho}) (orthos (momentum value)),
        Continue
      )
handleCollision _ _ (Util Destroy) _ = return $ Right ([], Continue)
-- Values annihilate each other if more than one value is present
handleCollision _ _ (Util _) (_ : _ : _) = return $ Right ([], Continue)
--------------------
handleCollision _ IoOperations {inputNumber} (Io InputDecimal) [value] =
  do
    n <- inputNumber
    return $
      Right
        ( [value {numericValue = n}],
          Continue
        )
handleCollision _ IoOperations {inputAscii} (Io InputAscii) [value] =
  do
    c <- inputAscii
    return $
      Right
        ( [value {numericValue = ord c}],
          Continue
        )
handleCollision _ IoOperations {output} (Io PrintDecimal) [value] =
  do
    output $ show (numericValue value)
    return $ Right ([value], Continue)
handleCollision _ IoOperations {output} (Io PrintAscii) [value] =
  let char = chr (numericValue value)
   in do
        output [char]
        return $ Right ([value], Continue)
handleCollision stepping IoOperations {debugger} (Io Break) [value] =
  if stepping
    then
      return $ Right ([value], Continue)
    else do
      action <- debugger
      return $ Right ([value], action)
handleCollision _ _ (Io Halt) [value] = return $ Left $ Halted (numericValue value)
handleCollision _ _ (Io Error) [value] = return $ Left $ Errored (numericValue value)
-- Values annihilate each other if more than one value is present
handleCollision _ _ (Io _) (_ : _ : _) = return $ Right ([], Continue)
--------------------
handleCollision stepping io (Unrecognised _) values = handleCollision stepping io (Util Default) values
--------------------
-- Default case: there are no active blocks, all blocks just manipulate incoming values
handleCollision _ _ _ [] = return $ Right ([], Continue)
