import Lexer as Lexer

import System.IO

-- This `main` allows us to generate a lexer binary, see cabal file
main :: IO ()
main = do code <- (hGetContents stdin)
          Lexer.emit $ Lexer.lex code
