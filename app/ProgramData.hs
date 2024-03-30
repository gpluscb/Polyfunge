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

data ControlFlowBlock = Conveyor Direction | Wait | Jump | VGate | HGate | Test | Not
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

associatedBlock :: Char -> Maybe Block
associatedBlock '^' = Just $ Control (Conveyor DirUp)
associatedBlock 'v' = Just $ Control (Conveyor DirDown)
associatedBlock '<' = Just $ Control (Conveyor DirLeft)
associatedBlock '>' = Just $ Control (Conveyor DirRight)
associatedBlock 'w' = Just $ Control Wait
associatedBlock '#' = Just $ Control Jump
associatedBlock '|' = Just $ Control VGate
associatedBlock '_' = Just $ Control HGate
associatedBlock '?' = Just $ Control Test
associatedBlock '!' = Just $ Control Not
---------------------------
associatedBlock '+' = Just $ BinaryArith Add
associatedBlock '-' = Just $ BinaryArith Sub
associatedBlock '/' = Just $ BinaryArith Div
associatedBlock '*' = Just $ BinaryArith Mul
associatedBlock '%' = Just $ BinaryArith Mod
associatedBlock 'g' = Just $ BinaryArith Gt
associatedBlock 'l' = Just $ BinaryArith Lt
---------------------------
associatedBlock 'z' = Just $ UnaryArith Zero
associatedBlock 'i' = Just $ UnaryArith Inc
associatedBlock 'd' = Just $ UnaryArith Dec
---------------------------
associatedBlock ' ' = Just $ Util Default
associatedBlock ':' = Just $ Util Dupe
associatedBlock 'x' = Just $ Util Destroy
---------------------------
associatedBlock 'N' = Just $ Io InputDecimal
associatedBlock 'I' = Just $ Io InputAscii
associatedBlock 'p' = Just $ Io PrintDecimal
associatedBlock 'P' = Just $ Io PrintAscii
associatedBlock 'b' = Just $ Io Break
associatedBlock 'h' = Just $ Io Halt
associatedBlock 'e' = Just $ Io Error
associatedBlock _ = Nothing