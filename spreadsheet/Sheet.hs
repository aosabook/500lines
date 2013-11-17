{-# LANGUAGE OverloadedStrings #-}
module Calc where
import Data.Map (Map, empty, insert)
import Data.String (IsString(..))
type Sheet = Map Coord Cell
data Coord = Coord { col :: Int, row :: Int } deriving (Show, Eq, Ord)
data Cell = Null | Numeric Int | Formula Coord deriving Show
data Cmd = SetValue Coord Int | SetFormula Coord Formula deriving (Show)
type Formula = Coord
instance IsString Coord where fromString (c:r) = Coord (fromEnum c - 64) (read r)
sheet = empty
execute sheet (SetValue coord val) = insert coord (Numeric val) sheet
execute sheet (SetFormula coord formula) = insert coord (Formula formula) sheet
main = print $ foldl execute sheet cmds
    where
    cmds = [SetValue "A1" 123, SetValue "A2" 456, SetFormula "B1" "A1"]
