module Lexer where

import Regex
import LexerUtils

import Data.Char (chr, isSpace)
import Data.List (stripPrefix, elemIndex, delete)
import Data.Maybe (catMaybes)
import Numeric (readOct, readHex)


-- TOKEN LOGIC; all logic relating to the tokens themselves
--
-- Guidance for defining tokens was provided by section 2 of the lexical spec,
-- (and in particular, section 2.2), as well as Matthew Might, who is eminently
-- useul as a compilers person.
data Tkn =
           -- "Core" tokens
             Newline
           | Indent
           | Dedent
           | Id String
           | Keyword String
           | Literal String
           | Punct String
           | Error String
           | Endmarker
           | Comment
           -- "Helper" tokens, mostly to help us process literals
           | StrLit String
           | StrIntLit String String  -- (multiline strings)
           | RStrLit String
           | RStrIntLit String String -- (multiline r-strings)
           | CmplxLit String
           | BinLit String
           | HexLit String
           | OctLit String
           | LineCont
           deriving (Eq)
instance Show Tkn where show = dispTkn  -- tells how to display tokens

-- turns tokens into strings
dispTkn :: Tkn -> String
-- "Core" tokens
dispTkn (Newline)        = "(NEWLINE)"
dispTkn (Indent)         = "(INDENT)"
dispTkn (Dedent)         = "(DEDENT)"
dispTkn (Id x)           = concat ["(ID \"", x, "\")"]
dispTkn (Keyword x)      = concat ["(KEYWORD ", x, ")"]
dispTkn (Literal x)      = concat ["(LIT ", x, ")"]
dispTkn (Punct x)        = concat ["(PUNCT \"", x, "\")"]
dispTkn (Error x)        = concat ["(ERROR \"", x, "\")"]
dispTkn (Endmarker)      = "(ENDMARKER)"
dispTkn (Comment)        = "(COMMENT)"
dispTkn (LineCont)       = "(LINECONT)"
-- "Helper" tokens
dispTkn (StrLit x)       = concat ["(LIT \"", x, "\")"]
dispTkn (StrIntLit x _)  = concat ["(STRING \"", x, "\")"]
dispTkn (RStrLit x)      = concat ["(rLIT \"", x, "\")"]
dispTkn (RStrIntLit x _) = concat ["(rLITint \"", (escbs x), "\")"]
dispTkn (CmplxLit x)     = concat ["(LIT ", x, ")"]
dispTkn (BinLit x)       = concat ["(LIT ", x, ")"]
dispTkn (HexLit x)       = concat ["(LIT ", x, ")"]
dispTkn (OctLit x)       = concat ["(LIT ", x, ")"]



-- LEXER API

-- OUTPUT. Takes a list of tokens, prints each on its own line
emit :: [Tkn] -> IO ()
emit tkns = mapM_ print tkns

-- INPUT. Takes a string and lexes it
lex :: String -> [Tkn]
lex input = concatStrLits (mapStrLits $ lexlines [0] [] $ lines input) []



-- LEXER CONTROL LOGIC

-- lexes all the lines in a file
-- indent stack -> parens stack -> lines in file -> tokens
lexlines :: [Int] -> [String] -> [String] -> [Tkn]
lexlines [] _ _ =
  -- the indent stack will always have at least one element inside, sometimes
  -- a 0; if it's not there, we error
  error "Indent stack can't be empty"
lexlines istack pstack (s:ss) =
  concat [tkns, nlTkn, lexlines istack' pstack'' ss]
  where
    (tkns, istack', pstack') = lexln istack pstack s
    nlTkn                    = nlTknUpdt pstack' tkns
    pstack''                 = pstackUpdt tkns pstack'
lexlines istack pstack []
  -- trailing parenthesis results in error
  | "\\" `elem` pstack = [Error "trailing backslash not allowed in file"]
  | istack == [0]      = [Endmarker]
  | otherwise          = (fst $ dedent istack 0) ++ [Endmarker]

-- lexes one line
-- indent stack -> parens stack -> a single line -> tokens ->
--    (tokens, new indent stack, new parens stack)
lexln :: [Int] -> [String] -> String -> ([Tkn], [Int], [String])
lexln istack [] ln = (concat [itkns, tkns'], istack', pstack')
  -- lex indents
  where
    noWs             = dropSpace ln
    curridts         = length $ takeWhile isSpace ln
    (tkns, pstack)   = tokenize noWs []
    (tkns', pstack') = stackUpdtWIndents tkns pstack
    -- INDENT tokens should not be emitted if line is empty
    (itkns, istack') | null tkns' = ([], istack)
                     | otherwise  = tknStackUpdt istack curridts
lexln istack pstack ln  = (tkns', istack, pstack'')
  -- don't lex indents
  where
    (tkns, pstack')   = tknizeOrLexCont pstack ln
    (tkns', pstack'') = stackUpdtWOIndents tkns pstack'

-- if parenthesis stack is empty, there is no line continuation.
-- if line cont is in tokens and not in paren stack, remove from tokens
-- if line cont is in tokens and not in paren stack, push to stack
-- if line cont is in paren stack and not in tokens, remove from stack
stackUpdtWOIndents :: [Tkn] -> [String] -> ([Tkn], [String])
stackUpdtWOIndents tkns pstack = case (LineCont `elemIndex` tkns) of
  Just x  -> isEol x
  Nothing -> (tkns' False False, pstack' False)
  where
    isEol n | n == ((length tkns) - 1) = (tkns' True False, pstack' True)
            | otherwise                = (tkns' False True, pstack' False)
    lcInPstack = "\\" `elem` pstack
    tkns' lcAtEol err | lcAtEol && lcInPstack ||
                        null pstack = delete LineCont tkns
                      | err         = (Error "backslash must be at eol") : tkns
                      | otherwise   = tkns
    pstack' lcAtEol | lcAtEol && not lcInPstack = "\\" : pstack
                    | not lcAtEol && lcInPstack = delete "\\" pstack
                    | otherwise                 = pstack

-- updates token and pstack states based on whether line continuation was (1)
-- present, and (2) at the EOL
stackUpdtWIndents :: [Tkn] -> [String] -> ([Tkn], [String])
stackUpdtWIndents tkns pstack = case (LineCont `elemIndex` tkns) of
  -- if there is no line continuation, return
  -- line continuations that do not occur at EOL result in error token
  -- line continuations at EOL result in normal return
  Just x  -> isEol x
  Nothing -> (tkns, pstack)
  where isEol n | n == ((length tkns) - 1) =
                    (delete LineCont tkns, "\\" : pstack)
                | otherwise =
                    ((Error "backslash must be at EOL") : tkns, pstack)

-- either tokenizes input string outright, or "continues" lexing b/c the
-- previous line was a multiline string
tknizeOrLexCont :: [String] -> String -> ([Tkn], [String])
tknizeOrLexCont pstack ln
  | head pstack == "\\"           = tokenize ln (tail pstack)
  | head pstack `elem` strDelim   = lexcont ln pstack
  | otherwise                     = tokenize ln pstack
  where strDelim = ["\'", "'''", "\"", "\"\"\"",
                    "r'", "r'''", "r\"", "r\"\"\""]

-- "continues" lexing string after a line continuation
lexcont :: String -> [String] -> ([Tkn], [String])
lexcont _ []        = error "to lex after line cont, we require a paren stack"
lexcont ln (par:ps) = (tkns, pstack')
  where
  (rx, postfix) = buildStrRx par
  (tkn, leftoverStr) = rxTknMap' rx ln postfix
  (leftoverTkns, pstack) = tokenize leftoverStr ps
  (tkns, pstack') = case tkn of
    StrLit _       -> (tkn : leftoverTkns, pstack)
    StrLit _      -> (tkn : leftoverTkns, pstack)
    StrIntLit _ _  -> ([tkn], par:ps)
    RStrIntLit _ _ -> ([tkn], par:ps)
    Error x        -> ([Error x], ps)
    _              -> ([Error "String Lexing error"], par:ps)



isShortstr :: String -> Bool
isShortstr qt | qt == "\""  = True
              | qt == "'"   = True
              | qt == "r\"" = True
              | qt == "r'"  = True
              | otherwise   = False

isRstr :: String -> Bool
isRstr qt | qt == "r\""     = True
          | qt == "r'"      = True
          | qt == "r'''"    = True
          | qt == "r\"\"\"" = True
          | otherwise       = False

-- builds regex based on whether it's a short or long string (or r-string)
buildStrRx :: String -> ((Regex, Tkn), [Char])
buildStrRx par = case (isShortstr par, isRstr par) of
    -- indexing b/c `shortstringcont` takes a character as arg
    (True, False)  -> ((shortstringcont (par !! 0), StrLit ""), par)
    (False, False) -> ((longstringcont par, StrLit ""), par)
    -- exclude 'r' character from strings
    (True, True)   -> ((shortstringcont (par !! 1), RStrLit ""), (tail par))
    (False, True)  -> ((longstringcont (tail par), RStrLit ""), (tail par))
    _              -> error "lexcont got called without a quote on the stack!"






-- HELPER FUNCTIONS

-- push e onto stack `st` if it is not in the stack already
pushIfUnique :: Eq a => a -> [a] -> [a]
pushIfUnique e st = case (e `elem` st) of
  True  -> st
  False -> e:st

-- if last token is a multiline string, add open to pstack to track it. ipstack
-- is the intermediate pstack
pstackUpdt :: [Tkn] -> [String] -> [String]
pstackUpdt [] pstack   = pstack
pstackUpdt tkns pstack = case last tkns of
  StrIntLit _ st  -> pushIfUnique st pstack
  RStrIntLit _ st -> pushIfUnique ('r':st) pstack
  _               -> pstack


-- adds newline token to stack if we encounter a non-escaped newline
nlTknUpdt :: [String] -> [Tkn] -> [Tkn]
nlTknUpdt _ []        = []  -- no tokens, then no nl
nlTknUpdt ["\\"] _    = []  -- escaped nl
nlTknUpdt (_:_) (_:_) = []  -- nested parentheses, then new nl
nlTknUpdt [] (x:xs)   = case last (x:xs) of
  StrIntLit _ _  -> []
  RStrIntLit _ _ -> []
  _              -> [Newline]  -- any token but long string is a nl







-- manages logic of popping pstack. skips over line continuations
parenpop :: [String] -> [String]
parenpop [] = []
parenpop (par:ps) | closesExp = concat [escs, ps']
                | otherwise = []
                  where (escs, (p':ps')) = span (\x -> x == "\\") (par:ps)
                        closesExp = p' == "}" || p' == "]" || p' == ")"


-- string -> paren stack -> (tokes list, new paren stack)
tokenize :: String -> [String] -> ([Tkn], [String])
tokenize s (par:ps) = case matchLongestTkn s' of
  Nothing -> case s' of
    "" -> ([], (par:ps))
    _  -> ([Error $ concat ["failed to tokenize the complete string. remainder: ", s']], [])
  (Just (tkn, s'')) -> case tkn of
    Punct "}" -> (tkn : tkns, pstack)
    Punct "]" -> (tkn : tkns, pstack)
    Punct ")" -> (tkn : tkns, pstack)
    _         -> lexOnTkn tkn s'' (par:ps)
    where (tkns, pstack) = tokenize s'' (parenpop (par:ps))
  where s' = dropSpace s
tokenize s [] = case matchLongestTkn s' of
  Nothing -> case s' of
    "" -> ([], [])
    _  -> ([Error $ concat ["failed to tokenize the complete string. remainder: ", s']], [])
  (Just (tkn, s'')) -> lexOnTkn tkn s'' []
  where s' = dropSpace s

-- provides context-dependent lexing based on given token
lexOnTkn :: Tkn -> String -> [String] -> ([Tkn], [String])
lexOnTkn tkn s pstack = case tkn of
  -- add new paren
  Punct "{"        -> stackUpdt "}"
  Punct "["        -> stackUpdt "]"
  Punct "("        -> stackUpdt ")"
  -- add new string
  StrIntLit _ "'"  -> stackUpdtBsEscd
  StrIntLit _ "\"" -> stackUpdtBsEscd
  -- check if \ appears at EOL
  LineCont         -> stackUpdtLineCont
  Comment          -> ([], pstack)
  _                -> stackUpdt'
  where stackUpdt punct = (tkn : tkns, pstack')
          where (tkns, pstack') = tokenize s (punct : pstack)
        stackUpdt'      = (tkn : tkns, pstack')
          where (tkns, pstack') = tokenize s pstack
        stackUpdtLineCont | not $ null s = ((Error "unescaped backslash in string") :
                                            tkn : tkns, pstack)
                          | null pstack  = (tkn : tkns, pstack')
                          | otherwise    = (tkns, pstack')
                            where (tkns, pstack') = tokenize s pstack
        stackUpdtBsEscd | not $ null s = ((Error "unescaped backslash in string") :
                                          tkn : tkns, pstack')
                        | otherwise    = (tkn : tkns, pstack')
                          where (tkns, pstack') = tokenize s pstack

matchLongestTkn :: String -> Maybe (Tkn, String)
matchLongestTkn s = result
  where
    intr   = catMaybes $ map (\x -> rxTknMap x s []) tknRxs
    maxln  = maximum $ map (\(_, x, _) -> x) intr
    result = case (filter (\(_, x, _) -> x == maxln) intr) of
      []    -> Nothing
      xs    -> Just ((\(tkn, _, str) -> (tkn, str)) (head xs))
    tknRxs = [(keyword, Keyword ""),  -- keywords must be matched before ids!!
              (indentifier, Id ""),
              (decimalinteger <|> floatnumber, Literal ""),
              (operator <|> delimiter, Punct ""),
              (oneOf "#", Comment),
              (stringliteral, StrLit ""),
              (rstringliteral, RStrLit ""),
              (imagnumber, CmplxLit ""),
              (bininteger, BinLit ""),
              (hexinteger, HexLit ""),
              (octinteger, OctLit ""),
              (seqOf "\\", LineCont)]

rxTknMap' :: (Regex, Tkn) -> String -> String -> (Tkn, String)
rxTknMap' (rx, tkn) s qt = case match rx s of
  Just (result, _, rest) -> (tkn', rest) where
    tkn' = case tkn of
      Id _       -> Id result
      Keyword _  -> Keyword result
      Literal _  -> Literal result
      Punct _    -> Punct result
      StrLit _   -> fmt result qt
      RStrLit _  -> fmt result qt
      CmplxLit _ -> CmplxLit $ '+' : (rep "j" "i" $ rep "J" "i" result )
      BinLit _   -> BinLit $ '#' : (tail result)
      HexLit _   -> HexLit $ '#' : (tail result)
      OctLit _   -> OctLit $ '#' : (tail result)
      _          -> tkn
  Nothing -> (Error "error lexing file", "")

-- Match regex and return corresponding token, match length, rest of string
rxTknMap :: (Regex, Tkn) -> String -> String -> Maybe (Tkn, Int, String)
rxTknMap (rx, tkn) s qt = case match rx s of
  Just (result, ln, rest) -> Just (tkn', ln, rest) where
    tkn' = case tkn of
      Id _       -> Id result
      Keyword _  -> Keyword result
      Literal _  -> Literal result
      Punct _    -> Punct result
      StrLit _   -> fmt result qt
      RStrLit _  -> fmt result qt
      CmplxLit _ -> CmplxLit $ '+' : (rep "j" "i" $ rep "J" "i" result )
      BinLit _   -> BinLit $ '#' : (tail result)
      HexLit _   -> HexLit $ '#' : (tail result)
      OctLit _   -> OctLit $ '#' : (tail result)
      _          -> tkn
  Nothing -> Nothing

fmt :: String -> String -> Tkn
fmt [] _ = StrLit ""
fmt s qt = result
  where
    (s', rstring, bsEol) | (head s) == 'r' = (tail s, True, '\\' == (last $ tail s))
                         | otherwise       = (s, False, '\\' == (last s))
    qln | null qt   = qPrefixLn s'
        | otherwise = length qt
    qchars | null qt  = take qln s'
           | otherwise = qt
    qtAtEol = drop (length s' - qln) s' == qchars
    (shortS, longS) | null qt   = (init $ drop qln s', drop qln s')
                    | bsEol     = (init s', init s')
                    | otherwise = (s', s')
    clsd | null qt   = drop qln $ take (length s' - qln) s'
         | otherwise = take (length s' - qln) s'
    result | qtAtEol && not rstring = StrLit clsd
           | qtAtEol && rstring     = RStrLit clsd
           | otherwise              = intermStrTkn shortS longS qchars bsEol rstring

-- builds intermediate strings for strings and r-strings
intermStrTkn :: String -> String -> String -> Bool -> Bool -> Tkn
intermStrTkn shortS longS qchars bsEol rstring =
  case (qchars, bsEol) of
       ("\"", False) -> Error "String not escaped"
       ("'", False)  -> Error "String not escaped"
       ("\"", True)  -> tokType shortS qchars
       ("'", True)   -> tokType shortS qchars
       (_, _)        -> tokType longS qchars
    where tokType | rstring   = RStrIntLit
                  | otherwise = StrIntLit

qPrefixLn :: String -> Int
qPrefixLn ('\'':'\'':'\'':_) = 3
qPrefixLn ('\"':'\"':'\"':_) = 3
qPrefixLn ('\'':_)           = 1
qPrefixLn ('"':_)            = 1
qPrefixLn _                  = 0

tknStackUpdt :: [Int] -> Int -> ([Tkn], [Int])
tknStackUpdt istack currIndent
  | lastIndent == currIndent = ([], istack)
  | lastIndent < currIndent  = ([Indent], currIndent : istack)
  | lastIndent > currIndent  = dedent istack currIndent
    where lastIndent = head istack

-- dedents the stack; indent stack -> current indentation -> (updt tkn & indnt stacks)
dedent :: [Int] -> Int -> ([Tkn], [Int])
dedent [] _ = ([],[])
dedent istack currIndent
  | i == currIndent = ([], istack)
  | i < currIndent  = ([Error "file incorrectly indented"], istack)
  | i > currIndent  = (concat [[Dedent], tkns], istack')
    where i = head istack
          (tkns, istack') = dedent (tail istack) currIndent

dropSpace :: String -> String
dropSpace s = dropWhile isSpace s

-- finds first instance of a substring in a string
rep :: String -> String -> String -> String
rep _ _ [] = []
rep toRepl subst xs@(y:ys) = case stripped of
  Just ys' -> subst ++ rep toRepl subst ys'
  Nothing  -> y : rep toRepl subst ys
  where stripped = stripPrefix toRepl xs

-- NOTE: next two functions come from somewhere else, but I forget where
-- escapes raw strings
escraw :: String -> String
escraw [] = []
escraw ('\\':'"':xs)  = '\\':'\\':'\\':'"':(escraw xs)
escraw ('\\':'\'':xs) = '\\':'\\':'\\':'\'':(escraw xs)
escraw ('\\':xs)      = '\\':'\\':(escraw xs)
escraw (x:xs)         = x:(escraw xs)

-- escapes backslashes
escbs :: String -> String
escbs  [] = []
escbs  ('\\':'\\':xs) = '\\':'\\':(escbs xs)
escbs ('\"':xs)       = '\\':'\"':(escbs xs)
escbs ('\'':xs)       = '\\':'\'':(escbs xs)
escbs  ('\\':s:xs)
  | s `elem` "UxbfNouartnv'\"" = '\\':s:(escbs xs)
  | otherwise                  = '\\':'\\':s:(escbs xs)
escbs (s:xs)          = s:(escbs xs)


-- maps rstrings to strings
mapStrLits :: [Tkn] -> [Tkn]
mapStrLits (tkn:ts) = case tkn of
  RStrLit s      -> StrLit (escraw s) : mapStrLits ts
  RStrIntLit s q -> StrIntLit ((escraw s) ++ "\\\\\\n") q : mapStrLits ts
  _              -> tkn : mapStrLits ts
mapStrLits _ = []

-- maps esc'd oct numbers to chars
mapEscdOct :: String -> String
mapEscdOct []              = []
mapEscdOct ('\\':a:b:c:xs)
  | allOcts = (chr $ fromIntegral $ fst . head . readOct $ a:b:c:[]):(mapEscdOct xs)
  | otherwise    = '\\':a:b:c:(mapEscdOct xs)
    where octals  = map (`elem` (map chr oct)) [a,b,c]
          allOcts = and octals
mapEscdOct (a:xs) = a:(mapEscdOct xs)

oct :: [Int]
oct = [48..55]

-- maps esc'd hex numbers to chars
mapEscdHex :: String -> String
mapEscdHex [] = []
mapEscdHex ('\\':a:b:c:xs)
  | allHexs = (chr $ fromIntegral $ fst . head . readHex $ b:c:[]):(mapEscdHex xs)
  | otherwise    = '\\':a:b:c:(mapEscdHex xs)
    where hexs    = map (`elem` (map chr hex)) [b,c]
          allHexs = and $ (a=='x') : hexs
mapEscdHex (a:xs) = a:(mapEscdHex xs)

hex :: [Int]
hex = ([48..57] ++ [97..102] ++ [65..70])

-- concatenates contiguous string literals
concatStrLits :: [Tkn] -> [Tkn] -> [Tkn]
concatStrLits (tkn:ts) remainder = case tkn of
    StrIntLit _ _ -> concatStrLits ts (tkn:remainder)
    StrLit _      -> catStrLit
    _             -> tkn : concatStrLits ts remainder
    where catStrLit | null remainder = (escTkn tkn) : concatStrLits ts []
                    | otherwise      = (escTkn $ StrLit $ showTkns (tkn:remainder)) : (concatStrLits ts [])
concatStrLits [] []    = []
concatStrLits [] (_:_) = [Error "unable to join string literals"]

-- a toString function for tokens
showTkns :: [Tkn] -> String
showTkns [] = ""
showTkns ts = concat $ reverse $ map tknToString ts

--
tknToString :: Tkn -> String
tknToString tkn = case tkn of
  StrLit s             -> s
  StrIntLit s "\'\'\'" -> concat [s, "\\n"]
  StrIntLit s "\"\"\"" -> concat [s, "\\n"]
  StrIntLit s ['\'']   -> s
  StrIntLit s ['"']    -> s


  _                    -> []

escTkn :: Tkn -> Tkn
escTkn tkn = case tkn of
  StrLit s      -> StrLit $ escbs $ mapEscdHex $ mapEscdOct s
  StrIntLit s _ -> StrIntLit (escbs $ mapEscdHex $ mapEscdOct s) ""
  _             -> tkn
