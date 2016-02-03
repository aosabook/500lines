module DerpInterface where

import Text.Derp as Derp

import Lexer


-- Turns our internal representation of tokens into tokens for the Derp lib


derpTkns :: [Lexer.Tkn] -> [Derp.Token]
derpTkns [] = []
derpTkns (x:xs) = (derpTkn x) : derpTkns xs

derpTkn :: Lexer.Tkn -> Derp.Token
-- "Core" tokens
derpTkn (Newline)        = Derp.Token "NEWLINE" "NEWLINE"
derpTkn (Indent)         = Derp.Token "INDENT"  "INDENT"
derpTkn (Dedent)         = Derp.Token "DEDENT"  "DEDENT"
derpTkn (Id x)           = Derp.Token "ID"      x
derpTkn (Keyword x)      = Derp.Token x         x
derpTkn (Literal x)      = Derp.Token "LIT"     x
derpTkn (Punct x)        = Derp.Token x         x
derpTkn (Error x)        = Derp.Token "ERROR"   x
derpTkn (Endmarker)      = Derp.Token "ENDMARKER" "ENDMARKER"
derpTkn (Comment)        = Derp.Token "COMMENT" "COMMENT"
-- "Helper" tokens
derpTkn (StrLit x)       = Derp.Token "STRING"  x
derpTkn (CmplxLit x)     = Derp.Token "LIT"     x
derpTkn (BinLit x)       = Derp.Token "LIT"     x
derpTkn (HexLit x)       = Derp.Token "LIT"     x
derpTkn (OctLit x)       = Derp.Token "LIT"     x
derpTkn _                = error "lexer outputs tokens that do not exist"

showNL :: [String] -> IO()
showNL (t:ts) = do putStrLn t
                   showNL ts
showNL [] = do putStrLn ""
