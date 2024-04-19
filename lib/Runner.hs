{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Runner where

import Control.Concurrent (threadDelay)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT, evalStateT, get, modify, state)
import qualified Data.Bifunctor
import Data.Char (chr, ord)
import qualified Data.List.NonEmpty
import ProgramData
import qualified Render (renderTick)
import System.Random (randomRIO)
import Utils

type InputNumberOperation m = m Int

standardInputOperation :: InputNumberOperation IO
standardInputOperation = do
  _ <- putStrLn "Awaiting numeric input"
  readNumber

type InputAsciiOperation m = m Char

standardInputAsciiOperation :: InputAsciiOperation IO
standardInputAsciiOperation = do
  _ <- putStrLn "Awaiting ascii input"
  readChar

type RandomInputOperation m = m Direction

standardRandomInputOperation :: RandomInputOperation IO
standardRandomInputOperation = do
  num <- randomRIO (0, 3) :: IO Int
  return $ case num of
    0 -> DirUp
    1 -> DirDown
    2 -> DirLeft
    3 -> DirRight
    _ -> error "Unreachable"

type OutputOperation m = Int -> m ()

standardOutputNumberOperation :: OutputOperation IO
standardOutputNumberOperation = print

debugOutputNumberOperation :: OutputOperation (StateT DebuggerState IO)
debugOutputNumberOperation num = modify $ \debuggerState -> debuggerState {output = output debuggerState ++ " " ++ (show num)}

type OutputAsciiOperation m = Int -> m ()

standardOutputAsciiOperation :: OutputOperation IO
standardOutputAsciiOperation c = putChar $ chr c

debugOutputAsciiOperation :: OutputOperation (StateT DebuggerState IO)
debugOutputAsciiOperation c = modify $ \debuggerState -> debuggerState {output = output debuggerState ++ [chr c]}

data ContinueAction = Continue | Abort

type InspectTickOperation m = TickInfo -> m ContinueAction

standardInspectTickOperation :: InspectTickOperation IO
standardInspectTickOperation _ = return Continue

data DebuggerState = DebuggerState
  { tickCount :: Int,
    stepping :: Bool,
    output :: String
  }

initialDebuggerState :: DebuggerState
initialDebuggerState = DebuggerState {tickCount = 0, stepping = True, output = ""}

debugInspectTickOperation :: Int -> InspectTickOperation (StateT DebuggerState IO)
debugInspectTickOperation microsecs TickInfo {debuggerTrigger, programState} = do
  DebuggerState {tickCount, stepping, output} <- get
  _ <- incrTickCount
  _ <- lift $ putStrLn $ Render.renderTick tickCount output programState
  if stepping || debuggerTrigger
    then do
      _ <- lift $ putStrLn "Program paused. Press enter or s to step once, c to continue, or a to abort"
      getAndApplyDebuggerAction
    else do
      _ <- lift $ threadDelay microsecs
      return Continue
  where
    getAndApplyDebuggerAction = do
      actionStr <- lift getLine
      case actionStr of
        "c" -> startContinuing
        "a" -> return Abort
        c | c `elem` ["s", ""] -> startStepping
        _ -> do
          _ <- lift $ putStrLn "That didn't work, try again"
          getAndApplyDebuggerAction
    incrTickCount = modify $ \debuggerState -> debuggerState {tickCount = succ $ tickCount debuggerState}
    startStepping = state $ \debuggerState -> (Continue, debuggerState {stepping = True})
    startContinuing = state $ \debuggerState -> (Continue, debuggerState {stepping = False})

data CustomOperations m = CustomOperations
  { inputNumber :: InputNumberOperation m,
    inputAscii :: InputAsciiOperation m,
    inputRandom :: RandomInputOperation m,
    outputNumber :: OutputOperation m,
    outputAscii :: OutputOperation m,
    inspectTick :: InspectTickOperation m
  }

standardOperations :: CustomOperations IO
standardOperations =
  CustomOperations
    { inputNumber = standardInputOperation,
      inputAscii = standardInputAsciiOperation,
      inputRandom = standardRandomInputOperation,
      outputNumber = standardOutputNumberOperation,
      outputAscii = standardOutputAsciiOperation,
      inspectTick = standardInspectTickOperation
    }

debugOperations :: Int -> CustomOperations (StateT DebuggerState IO)
debugOperations microsecs =
  CustomOperations
    { inputNumber = lift standardInputOperation,
      inputAscii = lift standardInputAsciiOperation,
      inputRandom = lift standardRandomInputOperation,
      outputNumber = debugOutputNumberOperation,
      outputAscii = debugOutputAsciiOperation,
      inspectTick = debugInspectTickOperation microsecs
    }

runNormal :: ProgramState -> IO EndOfProgram
runNormal programState = fst <$> run programState standardOperations

runDebug :: Int -> ProgramState -> IO EndOfProgram
runDebug microsecs programState = fst <$> evalStateT (run programState (debugOperations microsecs)) initialDebuggerState

run :: (Monad m) => ProgramState -> CustomOperations m -> m (EndOfProgram, TickInfo)
run programState = runRecursive TickInfo {debuggerTrigger = False, programState = programState}

runRecursive :: (Monad m) => TickInfo -> CustomOperations m -> m (EndOfProgram, TickInfo)
runRecursive tickInfo operations = do
  action <- inspectTick operations tickInfo
  case action of
    Continue -> do
      result <- tick tickInfo operations
      case result of
        Left end -> return (end, tickInfo)
        Right nextTickInfo -> runRecursive nextTickInfo operations
    Abort -> return (Aborted, tickInfo)

tick :: (Monad m) => TickInfo -> CustomOperations m -> m (Either EndOfProgram TickInfo)
tick tickInfo@TickInfo {programState} operations =
  let blocks = cells programState
      oldValues = values programState
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
      blocksWithMovedValues = concat $ blocksWithValues programState {values = movedValues}
      -- Do fusion
      blocksWithValuesAfterFusion =
        map (Data.Bifunctor.second doFusion) groupedByMomentum
        where
          groupedByMomentum =
            map (Data.Bifunctor.second (Data.List.NonEmpty.groupAllWith momentum)) blocksWithMovedValues
          doFusion =
            map $
              \values ->
                let updatedNumericValue = sum $ Data.List.NonEmpty.map numericValue values
                    updatedWaiting = all waiting values -- If one has not already waited, result should wait again
                 in (Data.List.NonEmpty.head values) {numericValue = updatedNumericValue, waiting = updatedWaiting}
      -- Handle collisions
      valuesAfterCollisionHandling =
        foldl
          ( \resultA resultB -> case (resultA, resultB) of
              -- Errors have highest priority
              (e@(Left (Errored _)), _) -> e
              (_, e@(Left (Errored _))) -> e
              -- Next priority is halted
              (h@(Left (Halted _)), _) -> h
              (_, h@(Left (Halted _))) -> h
              -- Next priority is any other termination
              -- Death can't be returned here but still
              (t@(Left _), _) -> t
              (_, t@(Left _)) -> t
              -- Lastly, we know both are okay
              (Right (valuesA, debuggerTriggerA), Right (valuesB, debuggerTriggerB)) ->
                Right $
                  ( valuesA ++ valuesB,
                    debuggerTriggerA || debuggerTriggerB
                  )
          )
          (Right ([], False))
          <$> collisionResults
        where
          collisionResults = mapM (uncurry (handleCollision operations)) blocksWithValuesAfterFusion
      -- A board with no values may exist for one tick, therefore check old values instead of new ones
      isDead = null oldValues
   in if isDead
        then return $ Left Died
        else
          fmap
            ( \(valuesAfterHandling, debuggerTrigger) ->
                (tickInfo {programState = programState {values = valuesAfterHandling}, debuggerTrigger = debuggerTrigger})
            )
            <$> valuesAfterCollisionHandling

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

handleCollision :: (Monad m) => CustomOperations m -> Block -> [Value] -> m (Either EndOfProgram ([Value], Bool))
--------------------
handleCollision _ (Control (Conveyor conveyorDir)) [value] =
  return $ Right ([value {momentum = conveyorDir}], False)
handleCollision CustomOperations {inputRandom} (Control Spinner) [value] = do
  newMomentum <- inputRandom
  return $ Right ([value {momentum = newMomentum}], False)
handleCollision _ (Control Wait) [value] =
  return $ Right ([value {waiting = not (waiting value)}], False)
-- Stepping onto the jump block does not do anything, only leaving it does
handleCollision _ (Control Jump) [value] =
  return $ Right ([value], False)
handleCollision _ (Control (Gate gateOrientation)) [value] =
  return $
    Right $
      ( if orientationFromDirection (momentum value) == gateOrientation
          then [value] -- Values in the direction of the gate get to pass
          else [value {momentum = mirror (momentum value)}], -- Others reflect
        False
      )
-- If two values align with the gate, two values do not align with the gate, or three or more values are present
-- they will be annihilated in default case
handleCollision x y@(Control (Gate gateOrientation)) [valueA, valueB]
  | orientationFromDirection (momentum valueB) == gateOrientation
      && orientationFromDirection (momentum valueA) /= gateOrientation =
      handleCollision x y [valueB, valueA] -- Reorder such that first value aligns with gate
  | orientationFromDirection (momentum valueA) == gateOrientation
      && orientationFromDirection (momentum valueB) /= gateOrientation =
      return $
        Right $
          ( if numericValue valueA == 0
              then [valueA, valueB] -- 0 allows passing of both
              else [valueA, valueB {momentum = mirror (momentum valueB)}], -- Everything else mirrors
            False
          )
handleCollision _ (Control Test) [value] =
  -- Keep 0, discard otherwise
  return $ Right (filter ((== 0) . numericValue) [value], False)
handleCollision _ (Control Not) [value] =
  -- Discard 0, keep otherwise
  return $ Right (filter ((/= 0) . numericValue) [value], False)
-- Values annihilate each other in all control blocks (except for gates) if more than one value is present
handleCollision _ (Control _) (_ : _ : _) = return $ Right ([], False)
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
        Right
          ( maybe
              [] -- Annihilation on invalid computation (e.g. division by 0)
              ( \(resA, resB) ->
                  [ valueA {numericValue = resA},
                    valueB {numericValue = resB}
                  ]
              )
              results,
            False
          )
handleCollision _ (BinaryArith _) [value] = return $ Right ([value], False)
-- Values annihilate each other if more than two inputs are given
handleCollision _ (BinaryArith _) (_ : _ : _ : _) = return $ Right ([], False)
--------------------
handleCollision _ (UnaryArith arithBlock) [value] =
  return $
    Right
      ( [value {numericValue = applyUnaryArith arithBlock (numericValue value)}],
        False
      )
-- Values annihilate each other if more than one value is present
handleCollision _ (UnaryArith _) (_ : _ : _) = return $ Right ([], False)
--------------------
handleCollision _ (Util Default) [value] = return $ Right ([value], False)
handleCollision _ (Util Dupe) [value] =
  return $
    Right
      ( value : map (\ortho -> value {momentum = ortho}) (orthos (momentum value)),
        False
      )
handleCollision _ (Util Destroy) _ = return $ Right ([], False)
-- Values annihilate each other if more than one value is present
handleCollision _ (Util _) (_ : _ : _) = return $ Right ([], False)
--------------------
handleCollision CustomOperations {inputNumber} (Io InputDecimal) [value] =
  do
    n <- inputNumber
    return $
      Right
        ( [value {numericValue = n}],
          False
        )
handleCollision CustomOperations {inputAscii} (Io InputAscii) [value] =
  do
    c <- inputAscii
    return $
      Right
        ( [value {numericValue = ord c}],
          False
        )
handleCollision CustomOperations {outputNumber} (Io PrintDecimal) [value] =
  do
    outputNumber $ numericValue value
    return $ Right ([value], False)
handleCollision CustomOperations {outputAscii} (Io PrintAscii) [value] =
  do
    outputAscii $ numericValue value
    return $ Right ([value], False)
handleCollision _ (Io Break) [value] =
  return $ Right ([value], True)
handleCollision _ (Io Halt) [value] = return $ Left $ Halted (numericValue value)
handleCollision _ (Io Error) [value] = return $ Left $ Errored (numericValue value)
-- Values annihilate each other if more than one value is present
handleCollision _ (Io _) (_ : _ : _) = return $ Right ([], False)
--------------------
handleCollision io (Unrecognised _) values = handleCollision io (Util Default) values
--------------------
-- Default case: there are no active blocks, all blocks just manipulate incoming values
handleCollision _ _ [] = return $ Right ([], False)
