{-
 - Contains mostly regular expressions for lexing Python. Follows extremely
 - closely with the Python lexical spec:
 - http://docs.python.org/3/reference/lexical_analysis.html
 -}
module LexerUtils where

import Data.Char
import Data.List ((\\))
import Regex


-- USEFUL CHARACTER CLASSES
-- all printable ascii alphanumeric characters
asciiPrintables :: String
asciiPrintables = map chr [33..126]

-- ascii whitespace
asciiws :: String
asciiws = " \r\n\t"

-- ascii printable characters + whitespace
ascii :: String
ascii = asciiPrintables ++ asciiws

{- NOTE TO MAINTAINERS ABOUT REGEXES:
   These regexes are (with the exception of the string regexes) taken
   *directly* from the python lexical spec, and they occur in *precisely* the
   order of the spec. If you have trouble here, go there for a complete
   treatment of why it is the way it is. -}

-- IDENTIFIERS
indentifier :: Regex
indentifier = id_start
          <.> star (id_start <|> ni)

id_start :: Regex
id_start = ll <|> lu <|> (oneOf "_")

-- following odd names come directly from the python lexical spec
-- [a-z]
ll :: Regex
ll = oneOf $ map chr [97..122]

-- [A-Z]
lu :: Regex
lu = oneOf $ map chr [65..90]

-- [0-9]
ni :: Regex
ni = oneOf $ map chr [48..57]


-- KEYWORDS
keyword :: Regex
keyword =   seqOf "False"
        <|> seqOf "None"
        <|> seqOf "True"
        <|> seqOf "and"
        <|> seqOf "as"
        <|> seqOf "assert"
        <|> seqOf "break"
        <|> seqOf "class"
        <|> seqOf "continue"
        <|> seqOf "def"
        <|> seqOf "del"
        <|> seqOf "elif"
        <|> seqOf "else"
        <|> seqOf "except"
        <|> seqOf "finally"
        <|> seqOf "for"
        <|> seqOf "from"
        <|> seqOf "global"
        <|> seqOf "if"
        <|> seqOf "import"
        <|> seqOf "in"
        <|> seqOf "is"
        <|> seqOf "lambda"
        <|> seqOf "nonlocal"
        <|> seqOf "not"
        <|> seqOf "or"
        <|> seqOf "pass"
        <|> seqOf "raise"
        <|> seqOf "return"
        <|> seqOf "try"
        <|> seqOf "while"
        <|> seqOf "with"
        <|> seqOf "yield"


-- STRING LITERALS
stringliteral :: Regex
stringliteral = (longstring "" "\"\"\"")
            <|> (longstring "" "'''")
            <|> (shortstring "" '\"')
            <|> (shortstring "" '\'')

rstringliteral :: Regex
rstringliteral = (longstring "rR" "\"\"\"")
             <|> (longstring "rR" "'''")
             <|> (shortstring "rR" '\"')
             <|> (shortstring "rR" '\'')

-- takes list of prefixes, a singlequote char or double char, and returns the
-- regex corresponding to that type of short string
shortstring :: [Char] -> Char -> Regex
shortstring "" qtype = lit qtype
                   <.> (star $ sourcechar qtype)
                   <.> (lit qtype <|> lit '\\')
shortstring prefixes qtype = oneOf prefixes
                       <.> lit qtype
                       <.> (star $ sourcechar qtype)
                       <.> (lit qtype <|> lit '\\')

-- matches the end of a short string with either double quote char or single
-- quote char.
shortstringcont :: Char -> Regex
shortstringcont qtype = (star $ sourcechar qtype)
                    <.> (lit qtype <|> lit '\\')

-- takes list of prefixes, a singlequote char or double char, and returns the
-- regex corresponding to that type of long string
longstring :: [Char] -> [Char] -> Regex
longstring "" qtype = seqOf qtype
                  <.> (star $ oneOf ascii)
                  <|> seqOf qtype
longstring prefixes qtype = oneOf prefixes
                        <.> seqOf qtype
                        <.> (star $ oneOf ascii)
                        <|> seqOf qtype

-- matches the end of long string with either three single quotes or three
-- double quotes
longstringcont :: [Char] -> Regex
longstringcont qtype = (star $ oneOf ascii)
                   <.> (lit '\n' <|> seqOf qtype)

-- Takes a character (either single or double quote), removes it, to produce
-- a regex matching chars that can appear in a string
sourcechar :: Char -> Regex
sourcechar qtype = (oneOf $ ascii \\ [qtype]) <|> (seqOf $ "\\" ++ [qtype])


-- INTEGER LITERALS
integer :: Regex
integer = decimalinteger
      <|> octinteger
      <|> hexinteger
      <|> bininteger

decimalinteger :: Regex
decimalinteger = (nonzerodigit <.> star ni)
               <|> (plus (seqOf "0"))

 -- [1-9]
nonzerodigit :: Regex
nonzerodigit = oneOf $ map chr [49..57]

digit :: Regex
digit = oneOf $ map chr [48..57]

octinteger :: Regex
octinteger = oneOf "0" <.> oneOf "oO" <.> plus octdigit

hexinteger :: Regex
hexinteger = oneOf "0" <.> oneOf "xX" <.> plus hexdigit

bininteger :: Regex
bininteger = oneOf "0" <.> oneOf "bB" <.> plus bindigit

-- [0-7]
octdigit :: Regex
octdigit = oneOf $ map chr [48..55]

-- [0-9a-fA-F]
hexdigit :: Regex
hexdigit = oneOf $ map chr ([48..57] ++ [97..102] ++ [65..70])

-- [0-1]
bindigit :: Regex
bindigit = oneOf $ map chr [48..49]


-- FLOATING POINT LITERALS
floatnumber :: Regex
floatnumber = pointfloat <|> exponentfloat

pointfloat :: Regex
pointfloat = (op intpart) <.> fraction <|> (intpart <.> oneOf ".")

exponentfloat :: Regex
exponentfloat = (intpart <|> pointfloat) <.> LexerUtils.exponent

intpart :: Regex
intpart = plus ni

fraction :: Regex
fraction = oneOf "." <.> intpart

exponent :: Regex
exponent = oneOf "eE"  <.> op (oneOf "+-") <.> intpart


-- IMAGINARY LITERALS
imagnumber :: Regex
imagnumber = (floatnumber <|> intpart) <.> oneOf "jJ"


-- OPERATORS
operator :: Regex
operator = oneOf "+-*/%&|^~<>"
       <|> seqOf "**"
       <|> seqOf "//"
       <|> seqOf "<<"
       <|> seqOf ">>"
       <|> seqOf "<="
       <|> seqOf ">="
       <|> seqOf "=="
       <|> seqOf "!="


-- DELIMITERS
delimiter :: Regex
delimiter = oneOf "()[]{},:.;@="
        <|> seqOf "+="
        <|> seqOf "-="
        <|> seqOf "*="
        <|> seqOf "/="
        <|> seqOf "//="
        <|> seqOf "%="
        <|> seqOf "&="
        <|> seqOf "|="
        <|> seqOf "^="
        <|> seqOf ">>="
        <|> seqOf "<<="
        <|> seqOf "**="
