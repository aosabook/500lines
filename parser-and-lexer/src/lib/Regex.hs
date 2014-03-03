-- Library to parse regexes with derivatives
-- based HEAVILY on David Darais' library that does the same thing

module Regex
       ( Regex
         , isNone, isNullable, lit, cat, (<.>), union, (<|>), star, eps
         , none, op, plus, oneOf, seqOf, der, match, lowerCaseChars
         , upperCaseChars, numberChars, inputChars, whitespaceChars
         , derRegexTests
       ) where
-- TODO: add exporting interface here

import Data.Char
import Test.HUnit

data Regex =
  RegexNone | RegexEps
  | RegexLit  Char
  | RegexCat  Regex Regex
  | RegexOr   Regex Regex
  | RegexStar Regex
  deriving (Eq, Show)

-- regular expression builders
none :: Regex
none = RegexNone

eps :: Regex
eps = RegexEps

lit :: Char -> Regex
lit = RegexLit

cat :: Regex -> Regex -> Regex
cat _ RegexNone        = RegexNone
cat RegexNone _        = RegexNone
cat x RegexEps         = x
cat RegexEps y         = y
cat (RegexCat x1 x2) y = cat x1 (RegexCat x2 y)
cat x y                = RegexCat x y

(<.>) :: Regex -> Regex -> Regex
(<.>) = cat

union :: Regex -> Regex -> Regex
union x RegexEps | isNullable x = x
union RegexEps y | isNullable y = y
union x RegexNone = x
union RegexNone y = y
union (RegexOr x1 x2) y = union x1 (RegexOr x2 y)
union x y = RegexOr x y

(<|>) :: Regex -> Regex -> Regex
(<|>) = union

star :: Regex -> Regex
star (RegexStar r) = RegexStar r
star r             = RegexStar r

op :: Regex -> Regex
op = union eps

plus :: Regex -> Regex
plus r = r <.> star r

oneOf :: String -> Regex
oneOf = foldr (union . lit) none

seqOf :: String -> Regex
seqOf = foldr (cat . lit) eps

-- regular expression matching

match :: Regex -> String -> Maybe (String, Int, String)
match rex _ | isNone rex      = Nothing
match rex [] | isNullable rex = Just ([], 0, [])
match _ [] | otherwise        = Nothing
match rex (x:xs) =
  case match (x `der` rex) xs of
    Just (matchResult, matchlen, rest) ->
      Just (x:matchResult, matchlen + 1, rest)
    Nothing ->
      if isNullable rex
        then Just ([], 0, x:xs)
        else Nothing

-- regular expression derivatives
der :: Char -> Regex -> Regex
der c (RegexLit rc) | c == rc   = eps
                    | otherwise = none
der c (RegexCat lhs rhs) | isNullable lhs = (der c lhs <.> rhs) <|> der c rhs
                         | otherwise      = der c lhs <.> rhs
der c (RegexOr lhs rhs) = der c lhs <|> der c rhs
der c (RegexStar e)     = der c e <.> star e
der _ RegexEps          = none
der _ RegexNone         = none


-- regular expression is equiv to none
isNone :: Regex -> Bool
isNone RegexNone          = True
isNone RegexEps           = False
isNone (RegexLit _)       = False
isNone (RegexStar _)      = False
isNone (RegexCat lhs rhs) = isNone lhs || isNone rhs
isNone (RegexOr lhs rhs)  = isNone lhs && isNone rhs

-- regular expression is nullable
isNullable :: Regex -> Bool
isNullable RegexNone          = False
isNullable RegexEps           = True
isNullable (RegexLit _)       = False
isNullable (RegexStar _)      = True
isNullable (RegexCat lhs rhs) = isNullable lhs && isNullable rhs
isNullable (RegexOr lhs rhs)  = isNullable lhs || isNullable rhs



lowerCaseChars :: String
lowerCaseChars = map chr [97..122]
upperCaseChars :: String
upperCaseChars = map chr [65..90]
numberChars :: String
numberChars = map chr [48..57]
inputChars :: String
inputChars = map chr [33..126]
whitespaceChars :: String
whitespaceChars = " \r\n\t"


-- Regex Testing
derRegexTests :: Test
derRegexTests = TestList
  [ TestCase . assertBool "" $ seqOf "abc" `match` "abc" == Just ("abc", 3, "")
  , TestCase . assertBool "" $ seqOf "ab" `match` "abc" == Just ("ab", 2, "c")
  , TestCase . assertBool "" $ seqOf "ab" `match` "acb" == Nothing
  , TestCase . assertBool "" $ seqOf "" `match` "abc" == Just ("", 0, "abc")
  , TestCase . assertBool "" $ star (seqOf "a") `match` "aaaaab" == Just ("aaaaa", 5, "b")
  , TestCase . assertBool "" $ star (seqOf "a") `match` "bbbbba" == Just ("", 0, "bbbbba")
  , TestCase . assertBool "" $ op (seqOf "a") `match` "aaaaab" == Just ("a", 1, "aaaab")
  , TestCase . assertBool "" $ op (seqOf "a") `match` "bbbbba" == Just ("", 0, "bbbbba")
  , TestCase . assertBool "" $ plus (seqOf "a") `match` "bbbbba" == Nothing
  , TestCase . assertBool "" $ plus (seqOf "a") `match` "aaaaab" == Just ("aaaaa", 5, "b")
  , TestCase . assertBool "" $ seqOf "abc" <.> seqOf "xyz" `match` "abcxyzabcxyz" == Just ("abcxyz", 6, "abcxyz")
  , TestCase . assertBool "" $ seqOf "abc" <|> seqOf "xyz" `match` "abcxyzabcxyz" == Just ("abc", 3, "xyzabcxyz")
  , TestCase . assertBool "" $ seqOf "abc" <|> seqOf "xyz" `match` "xyzabcxyzabc" == Just ("xyz", 3, "abcxyzabc")
  , TestCase . assertBool "" $ star (seqOf "abc") <.> seqOf "x" `match` "abcabcd" == Nothing
  , TestCase . assertBool "" $ star (seqOf "abc") <.> seqOf "x" `match` "abcabcxd" == Just ("abcabcx", 7, "d")
  , TestCase . assertBool "" $ star (seqOf "abc") <.> op (seqOf "x") `match` "abcabcd" == Just ("abcabc", 6, "d")
  ]