{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}
module Calc where
import Data.Map (Map, empty, insert, (!))
import Data.String (IsString(..))
import Data.Text (Text)
import Data.Attoparsec.Expr
import Data.Attoparsec.Text
import Control.Applicative ((<|>))

type Sheet = Map Coord Cell
data Coord = Coord { col :: Int, row :: Int } deriving (Eq, Ord)
data Cell = Null | Numeric Number | Formula Formula deriving Show
data Cmd = SetValue   { coord :: Coord, val :: Number }
         | SetFormula { coord :: Coord, formula :: Formula } deriving Show
type Formula = Text
instance IsString Coord where fromString (c:r) = Coord (fromEnum c-64) (read r)
instance Show Coord where show Coord{col,row} = toEnum (col+64) : show row

main = print . recalc . foldl execute empty $
    [ SetValue "A1" 123, SetValue "A2" 456
    , SetFormula "B1" "-A1-A2", SetFormula "B2" "$A1*-$B1" ]

execute = flip $ \x -> insert (coord x) $ case x of
    SetValue{val}       -> Numeric val
    SetFormula{formula} -> Formula formula

recalc s = fmap calc s where
    calc (Formula c) = maybe Null Numeric . maybeResult $ feed (parse expr c) ""
    calc x = x
    expr = buildExpressionParser table term
    term = ("(" .*> expr <*. ")") <|> ref <|> number
    ref = do option "" "$"; c <- letter; r <- many1 digit
             return $ case recalc s ! fromString (c:r) of Numeric n -> n; _ -> 0
    table = [ [prefix "-" negate, prefix "+" id ]
            , [binary "*" (*) AssocLeft, binary "/" (/) AssocLeft ]
            , [binary "+" (+) AssocLeft, binary "-" (-) AssocLeft ] ]
    binary  name fun assoc = Infix ( string name >> return fun ) assoc
    prefix  name fun       = Prefix ( string name >> return fun )
