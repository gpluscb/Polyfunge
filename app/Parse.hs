module Parse where

import Runner

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
associatedBlock '~' = Just $ Util Crossing
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
