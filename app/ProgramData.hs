{-# LANGUAGE NamedFieldPuns #-}

module ProgramData where

import Utils

data ProgramState = ProgramState
  { cells :: [[Block]],
    values :: [Value]
  }
  deriving (Read, Show, Eq)

isValid :: ProgramState -> Bool
isValid ProgramState {cells, values} =
  let (width, height) = gridDimensions cells
      cellsValid = all ((== width) . length) cells
      valuesValid = all (isInBounds (width, height) . position) values
   in cellsValid && valuesValid

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

data UtilBlock = Default | Dupe | Destroy
  deriving (Read, Show, Eq, Enum)

data IoBlock = Input | PrintDecimal | PrintAscii | Break | Halt | Error
  deriving (Read, Show, Eq, Enum)

data EndOfProgram = Died | Halted Int | Errored Int
  deriving (Read, Show, Eq)

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
associatedChar (Util Dupe) = ':'
associatedChar (Util Destroy) = 'x'
---------------------------
associatedChar (Io Input) = 'I'
associatedChar (Io PrintDecimal) = 'p'
associatedChar (Io PrintAscii) = 'P'
associatedChar (Io Break) = 'b'
associatedChar (Io Halt) = 'h'
associatedChar (Io Error) = 'e'

associatedBlock :: Char -> Maybe Block
associatedBlock '^' = Just $ Control (Conveyor DirUp)
associatedBlock 'v' = Just $ Control (Conveyor DirDown)
associatedBlock '<' = Just $ Control (Conveyor DirLeft)
associatedBlock '>' = Just $ Control (Conveyor DirRight)
associatedBlock 'w' = Just $ Control Wait
associatedBlock '#' = Just $ Control Jump
associatedBlock '|' = Just $ Control VGate
associatedBlock '_' = Just $ Control HGate
associatedBlock '?' = Just $ Control Question
---------------------------
associatedBlock '+' = Just $ BinaryArith Add
associatedBlock '-' = Just $ BinaryArith Sub
associatedBlock '*' = Just $ BinaryArith Mul
associatedBlock '/' = Just $ BinaryArith Div
associatedBlock '%' = Just $ BinaryArith Mod
associatedBlock 'g' = Just $ BinaryArith Gt
associatedBlock 'l' = Just $ BinaryArith Lt
---------------------------
associatedBlock 'z' = Just $ UnaryArith Zero
---------------------------
associatedBlock ' ' = Just $ Util Default
associatedBlock ':' = Just $ Util Dupe
associatedBlock 'x' = Just $ Util Destroy
---------------------------
associatedBlock 'I' = Just $ Io Input
associatedBlock 'p' = Just $ Io PrintDecimal
associatedBlock 'P' = Just $ Io PrintAscii
associatedBlock 'b' = Just $ Io Break
associatedBlock 'h' = Just $ Io Halt
associatedBlock 'e' = Just $ Io Error
associatedBlock _ = Nothing