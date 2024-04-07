{-# LANGUAGE NamedFieldPuns #-}

module ProgramData where

import Utils

data TickInfo = TickInfo
  { debuggerTrigger :: Bool,
    programState :: ProgramState
  }

data ProgramState = ProgramState
  { values :: [Value],
    cells :: [[Block]]
  }
  deriving (Read, Show, Eq)

constructProgramState :: [Value] -> [[Block]] -> ProgramState
constructProgramState values blocks =
  let paddedBlocks = padToEqualWidth (Util Default) blocks
   in ProgramState {values = values, cells = paddedBlocks}

isValid :: ProgramState -> Bool
isValid ProgramState {values, cells} =
  let (width, height) = gridDimensions cells
      cellsValid = all ((== width) . length) cells
      valuesValid = all (isInBounds (width, height) . position) values
   in cellsValid && valuesValid

blocksWithValues :: ProgramState -> [[(Block, [Value])]]
blocksWithValues ProgramState {values, cells} =
  zipWith
    ( \y row ->
        zipWith
          (\x cell -> (cell, valuesAt x y))
          [0 ..]
          row
    )
    [0 ..]
    cells
  where
    valuesAt x y = filter (\Value {position} -> position == (x, y)) values

data Value = Value
  { position :: (Int, Int),
    numericValue :: Int,
    momentum :: Direction,
    waiting :: Bool
  }
  deriving (Read, Show, Eq)

data Direction = DirUp | DirDown | DirLeft | DirRight
  deriving (Read, Show, Ord, Eq, Enum)

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

data Orientation = Horizontal | Vertical
  deriving (Read, Show, Ord, Eq, Enum)

orientationFromDirection :: Direction -> Orientation
orientationFromDirection DirUp = Vertical
orientationFromDirection DirDown = Vertical
orientationFromDirection DirLeft = Horizontal
orientationFromDirection DirRight = Horizontal

rotate :: Orientation -> Orientation
rotate Horizontal = Vertical
rotate Vertical = Horizontal

data Block = Control ControlFlowBlock | BinaryArith BinaryArithBlock | UnaryArith UnaryArithBlock | Util UtilBlock | Io IoBlock | Unrecognised Char
  deriving (Read, Show, Eq)

data ControlFlowBlock = Conveyor Direction | Spinner | Wait | Jump | Gate Orientation | Test | Not
  deriving (Read, Show, Eq)

data BinaryArithBlock = Add | Sub | Div | Mul | Mod | Gt | Lt
  deriving (Read, Show, Eq, Enum)

applyBinaryArith :: BinaryArithBlock -> Int -> Int -> Maybe Int
applyBinaryArith Add a b = Just $ a + b
applyBinaryArith Sub a b = Just $ a - b
applyBinaryArith Div a b = if b == 0 then Nothing else Just $ quot a b
applyBinaryArith Mul a b = Just $ a * b
applyBinaryArith Mod a b = Just $ mod a b
applyBinaryArith Gt a b = Just $ if a > b then 0 else 1
applyBinaryArith Lt a b = Just $ if a < b then 0 else 1

data UnaryArithBlock = Zero | Inc | Dec
  deriving (Read, Show, Eq, Enum)

applyUnaryArith :: UnaryArithBlock -> Int -> Int
applyUnaryArith Zero _ = 0
applyUnaryArith Inc x = succ x
applyUnaryArith Dec x = pred x

data UtilBlock = Default | Dupe | Destroy
  deriving (Read, Show, Eq, Enum)

data IoBlock = InputDecimal | InputAscii | PrintDecimal | PrintAscii | Break | Halt | Error
  deriving (Read, Show, Eq, Enum)

data EndOfProgram = Died | Aborted | Halted Int | Errored Int
  deriving (Read, Show, Eq)

associatedChar :: Block -> Char
associatedChar (Control (Conveyor DirUp)) = '^'
associatedChar (Control (Conveyor DirDown)) = 'v'
associatedChar (Control (Conveyor DirLeft)) = '<'
associatedChar (Control (Conveyor DirRight)) = '>'
associatedChar (Control Spinner) = '@'
associatedChar (Control Wait) = 'w'
associatedChar (Control Jump) = '#'
associatedChar (Control (Gate Vertical)) = '|'
associatedChar (Control (Gate Horizontal)) = '_'
associatedChar (Control Test) = '?'
associatedChar (Control Not) = '!'
---------------------------
associatedChar (BinaryArith Add) = '+'
associatedChar (BinaryArith Sub) = '-'
associatedChar (BinaryArith Div) = '/'
associatedChar (BinaryArith Mul) = '*'
associatedChar (BinaryArith Mod) = '%'
associatedChar (BinaryArith Gt) = 'g'
associatedChar (BinaryArith Lt) = 'l'
---------------------------
associatedChar (UnaryArith Zero) = 'z'
associatedChar (UnaryArith Inc) = 'i'
associatedChar (UnaryArith Dec) = 'd'
---------------------------
associatedChar (Util Default) = ' '
associatedChar (Util Dupe) = ':'
associatedChar (Util Destroy) = 'x'
---------------------------
associatedChar (Io InputDecimal) = 'N'
associatedChar (Io InputAscii) = 'I'
associatedChar (Io PrintDecimal) = 'p'
associatedChar (Io PrintAscii) = 'P'
associatedChar (Io Break) = 'b'
associatedChar (Io Halt) = 'h'
associatedChar (Io Error) = 'e'
---------------------------
associatedChar (Unrecognised c) = c

associatedBlock :: Char -> Block
associatedBlock '^' = Control $ Conveyor DirUp
associatedBlock 'v' = Control $ Conveyor DirDown
associatedBlock '<' = Control $ Conveyor DirLeft
associatedBlock '>' = Control $ Conveyor DirRight
associatedBlock '@' = Control Spinner
associatedBlock 'w' = Control Wait
associatedBlock '#' = Control Jump
associatedBlock '|' = Control $ Gate Vertical
associatedBlock '_' = Control $ Gate Horizontal
associatedBlock '?' = Control Test
associatedBlock '!' = Control Not
---------------------------
associatedBlock '+' = BinaryArith Add
associatedBlock '-' = BinaryArith Sub
associatedBlock '/' = BinaryArith Div
associatedBlock '*' = BinaryArith Mul
associatedBlock '%' = BinaryArith Mod
associatedBlock 'g' = BinaryArith Gt
associatedBlock 'l' = BinaryArith Lt
---------------------------
associatedBlock 'z' = UnaryArith Zero
associatedBlock 'i' = UnaryArith Inc
associatedBlock 'd' = UnaryArith Dec
---------------------------
associatedBlock ' ' = Util Default
associatedBlock ':' = Util Dupe
associatedBlock 'x' = Util Destroy
---------------------------
associatedBlock 'N' = Io InputDecimal
associatedBlock 'I' = Io InputAscii
associatedBlock 'p' = Io PrintDecimal
associatedBlock 'P' = Io PrintAscii
associatedBlock 'b' = Io Break
associatedBlock 'h' = Io Halt
associatedBlock 'e' = Io Error
associatedBlock c = Unrecognised c