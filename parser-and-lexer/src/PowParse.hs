import System.IO (hGetContents, stdin)
import qualified Lexer as Lexer
import qualified Parser as Parser
import DerpInterface

import Data.Set

-- This `main` allows us to generate a compiler binary, see cabal file
main :: IO ()
main = do code <- (hGetContents stdin)
          let res = toList $ Parser.parseFile $ Lexer.lex code
          case res of
            [] -> putStrLn "#f"
            x  -> showNL x
