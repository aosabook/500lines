-- A lot of this is copied basically verbatim from the full python grammar
-- located at http://docs.python.org/3/reference/grammar.html
-- For example, the function `fileInput` is actually just a rule in the Python
-- grammar. Nearly everything also appears in the order it does in the grammar.
module Parser (parseFile) where

import Prelude hiding (break, exp, exponent, id, lines, not, otherwise, return)
import Data.String.Utils (join)
import Data.Set (Set)
import Text.Derp

import DerpInterface
import EmitParseTree
import qualified Lexer


-- PUBLIC-FACING PARSER API FUNCTIONS

parseFile :: [Lexer.Tkn] -> Set String
parseFile tkns = runParse fileInput $ derpTkns tkns

-- PRIVATE "HELPER" FUNCTIONS FOR THE PARSER
-- These functions mostly correspond to the grammar rules in the Python
-- grammar, but are effectively useless outside of this module. Thus they are
-- private.

-- corresponds to `file_input` in grammar
fileInput :: Parser String
fileInput = lines <~> endOfFile ==> emitProgram
  where lines     = lineCfg
        endOfFile = ter "ENDMARKER"

lineCfg :: Parser String
lineCfg = emptyLine
          <|> newline <~> moreLines ==> emitNl
          <|> stmt <~> moreLines    ==> emitLine
  where emptyLine = eps ""
        newline = ter "NEWLINE"
        moreLines = lineCfg

-- corresponds to `funcdef` in grammar
funcdef :: Parser String
funcdef = def <~> id <~> parameters <~> colon <~> body ==> emitFuncdef
  where def   = ter "def"
        id    = ter "ID"
        colon = ter ":"
        body  = suite

-- corresponds to `parameters` in grammar
parameters :: Parser String
parameters = openParen <~> zeroPlusParams <~> closeParen ==> emitParams
  where openParen  = ter "("
        closeParen = ter ")"

zeroPlusParams :: Parser String
zeroPlusParams = noParams
                 <|> id <~> restOfIds ==> emitParamList
                 <|> comma
  where noParams  = eps ""
        id        = ter "ID"
        comma     = ter ","
        restOfIds = noParams
                    <|> comma <~> id <~> restOfIds ==> emitRestOfParams

-- corresponds to `stmt` in grammar
stmt :: Parser String
stmt = simpleStmt <|> compoundStmt

-- corresponds to `simple_stmt` in grammar
-- simple statements are either terminated with newline or semicolon
simpleStmt :: Parser String
simpleStmt = smallStmt <~> zeroPlusSmallStmts <~> endOfStmts ==> emitSimpleStmt
  where noMoreStmts = eps ""
        semicolon   = ter ";"
        newline     = ter "NEWLINE"
        endOfStmts  = (noMoreStmts <~> newline)
                      <|> (semicolon <~> newline)

zeroPlusSmallStmts :: Parser String
zeroPlusSmallStmts = noMoreStmts
                     <|> moreSmallStmts
  where noMoreStmts    = eps ""
        semicolon      = ter ";"
        moreSmallStmts = semicolon <~> smallStmt <~> moreSmallStmts
                          ==> emitSmallStmts

-- corresponds to `small_stmt` in grammar
smallStmt :: Parser String
smallStmt = exprStmt
            <|> delStmt
            <|> passStmt
            <|> flowStmt
            <|> globalStmt
            <|> nonlocalStmt
            <|> assertStmt

-- corresponds to `expr_stmt` in grammar
-- augmented assignment is stuff like `+=` and `*=`. A "test" is an assignment
-- with a conditional, eg, `x = 1 if 1 == y else 2`.

-- TODO: REFACTOR THIS TO USE DIFFERENT FUNCTIONS AND STUFF
exprStmt :: Parser String
exprStmt = manyTests <~> augassign <~> manyTests ==> emitAugAssignStmt
           <|> manyTests <~> equals <~> testOrTests ==> emitAssignStmt
           <|> testOrTests ==> emitExprStmt
    where equals = ter "="

-- corresponds to `augassign` in grammar
augassign :: Parser String
augassign = ter "+="
            <|> ter "-="
            <|> ter "*="
            <|> ter "/="
            <|> ter "%="
            <|> ter "&="
            <|> ter "|="
            <|> ter "^="
            <|> ter "<<="
            <|> ter ">>="
            <|> ter "**="
            <|> ter "//="

-- corresponds to `del_stmt` in grammar
delStmt :: Parser String
delStmt = del <~> starExpr ==> emitDelStmt
  where del = ter "del"

-- corresponds to `pass_stmt` in grammar
passStmt :: Parser String
passStmt = pass ==> emitPassStmt
  where pass = ter "pass" 

-- corresponds to `flow_stmt` in grammar
flowStmt :: Parser String
flowStmt = breakStmt <|> continueStmt <|> returnStmt <|> raiseStmt

-- corresponds to `break_stmt` in grammar
breakStmt :: Parser String
breakStmt = break ==> emitBreakStmt
  where break = ter "break"

-- corresponds to `continue_stmt` in grammar
continueStmt :: Parser String
continueStmt = continue ==> emitContinueStmt
  where continue = ter "continue"

-- corresponds to `return_stmt` in grammar
returnStmt :: Parser String
returnStmt = return <~> returnExpr ==> emitReturnStmt
  where return = ter "return"

-- whatever comes after `return`, eg `return "cows"`
returnExpr :: Parser String
returnExpr = none
             <|> exp
  where none = eps ""
        exp  = manyTests

-- corresponds to `raise_stmt` in grammar
raiseStmt :: Parser String
raiseStmt = raise <~> exceptionTypesAndVars ==> emitRaiseStmt
  where raise                 = ter "raise"
        exceptionTypesAndVars = raiseClause

-- matches whatever comes after `raise`, eg `raise ValueError("too many cows")`
-- `raise` with no args re-raises an exception in the except suite.
raiseClause :: Parser String
raiseClause = reRaiseException
              <|> raiseNewException
  where reRaiseException  = eps ""
        raiseNewException = test <~> raiseFrom ==> emitRaiseNewException

-- eg, `raise ValueError from msg`
raiseFrom :: Parser String
raiseFrom = fromNowhere
            <|> from <~> test ==> emitFromStmt
  where fromNowhere  = eps ""
        from         = ter "from"
        

-- corresponds to `global_stmt` in grammar
globalStmt :: Parser String
globalStmt = global <~> id <~> zeroPlusIds ==> emitGlobalStmt
  where global = ter "global"
        id     = ter "ID"

zeroPlusIds :: Parser String
zeroPlusIds = noMoreIds
              <|> comma <~> id <~> zeroPlusIds ==> emitRestOfIds
  where noMoreIds = eps ""
        comma     = ter ","
        id        = ter "ID"

-- corresponds to `nonlocal_stmt` in grammar
nonlocalStmt :: Parser String
nonlocalStmt = nonlocal <~> id <~> zeroPlusIds ==> emitNonlocalStmt
  where nonlocal = ter "nonlocal"
        id       = ter "ID"

-- corresponds to `assert_stmt` in grammar
assertStmt :: Parser String
assertStmt = assert <~> test <~> zeroPlusTests ==> emitAssertStmt
  where assert = ter "assert"

zeroPlusTests :: Parser String
zeroPlusTests = noMoreTests
                <|> comma <~> test ==> emitRestTests
  where noMoreTests = eps ""
        comma       = ter ","

-- corresponds to `compound_stmt` in grammar
compoundStmt :: Parser String
compoundStmt = ifStmt
               <|> whileStmt
               <|> forStmt
               <|> tryStmt
               <|> funcdef

-- corresponds to `if_stmt` in grammar
ifStmt :: Parser String
ifStmt = ifKeyword <~> test <~> colon <~> block <~> otherwise ==> emitIfStmt
  where ifKeyword = ter "if"
        colon     = ter ":"
        block     = suite
        otherwise = zeroOrMoreElifs <~> elseClause

elseClause :: Parser String
elseClause = noElseClause
             <|> elseKeyword <~> colon <~> block ==> emitElseClause
  where noElseClause = eps ""
        elseKeyword = ter "else"
        colon       = ter ":"
        block       = suite

zeroOrMoreElifs :: Parser String
zeroOrMoreElifs = noMoreElifs
                  <|> elif <~> test <~> colon <~> block <~> zeroOrMoreElifs
                  ==> emitElifs
  where noMoreElifs = eps "" 
        elif        = ter "elif"
        colon       = ter ":"
        block       = suite

-- corresponds to `while_stmt` in grammar
whileStmt :: Parser String
whileStmt = while <~> test <~> colon <~> block <~> whileElseClause
            ==> emitWhileStmt
  where while = ter "while"
        colon = ter ":"
        block = suite

whileElseClause :: Parser String
whileElseClause = noElseClause
                  <|> elseKeyword <~> colon <~> block ==> emitWhileElseClause
  where noElseClause = eps ""
        elseKeyword  = ter "else"
        colon        = ter ":"
        block        = suite

-- corresponds to `for_stmt` in grammar
forStmt :: Parser String
forStmt = for <~> id <~> inKeywrd <~> test <~> colon <~> block <~> forElseClause
          ==> emitForStmt
  where for      = ter "for"
        id       = ter "ID"
        inKeywrd = ter "in"
        colon    = ter ":"
        block    = suite

forElseClause :: Parser String
forElseClause = noElseClause
                <|> elseKeyword <~> colon <~> block ==> emitForElseClause
  where noElseClause = eps ""
        elseKeyword  = ter "else"
        colon        = ter ":"
        block        = suite

-- corresponds to `try_stmt` in grammar
tryStmt :: Parser String
tryStmt = try <~> colon <~> block <~> exceptionHandlers ==> emitTryStmt
  where try               = ter "try"
        colon             = ter ":"
        block             = suite
        onlyAFinallyBlock = finallyTryBlock
        exceptionHandlers = catchAndFinallyBlocks <|> onlyAFinallyBlock

-- used to emit finally block occurring right after a try block
finallyTryBlock :: Parser String
finallyTryBlock = finally <~> colon <~> block ==> emitFinallyTryBlock
  where finally = ter "finally"
        colon   = ter ":"
        block   = suite

finallyCatchBlock :: Parser String
finallyCatchBlock = noFinallyBlock
                    <|> finally <~> colon <~> block ==> emitFinallyCatchBlock
  where noFinallyBlock = eps ""
        finally        = ter "finally"
        colon          = ter ":"
        block          = suite

catchAndFinallyBlocks :: Parser String
catchAndFinallyBlocks = exceptClause <~> colon <~> block <~> zeroPlusExcepts
                        <~> elseBlock <~> finallyBlock ==> emitExceptElseFinally
  where -- no extra excepts
        colon        = ter ":"
        block        = suite
        elseBlock    = exceptElseClause ==> emitFailIfNotParsed
        finallyBlock = finallyCatchBlock ==> emitFailIfNotParsed

exceptElseClause :: Parser String
exceptElseClause = noElseClause
                   <|> elseKeyword <~> colon <~> block ==> emitExceptElseClause
  where noElseClause = eps ""
        elseKeyword  = ter "else"
        colon        = ter ":"
        block        = suite

zeroPlusExcepts :: Parser String
zeroPlusExcepts = noMoreExcepts
                  <|> exceptClause <~> colon <~> block <~> zeroPlusExcepts
                  ==> emitExceptClauses
  where noMoreExcepts = eps ""
        colon         = ter ":"
        block         = suite

-- corresponds to `except_clause` in grammar
exceptClause :: Parser String
exceptClause = except <~> exceptType ==> emitExceptClause
  where except = ter "except"

-- matches what sort of exception we're catching, eg, in `except ValueError:`,
-- we'd matech `ValueError`.
exceptType :: Parser String
exceptType = catchAllExceptions
             <|> catchSpecificExceptions ==> emitExceptType
  where catchAllExceptions      = eps ""  -- `except:` will catch all excptns
        catchSpecificExceptions = test <~> exceptVars

-- matches vars in exception declaration, eg, in `except ValueError as e:` we
-- we would match `as e`
exceptVars :: Parser String
exceptVars = noVars
             <|> as <~> id ==> emitExceptVars
  where noVars = eps ""
        as     = ter "as"
        id     = ter "ID"

-- corresponds to `suite` in grammar
suite :: Parser String
suite = simpleStmt
        <|> newline <~> indent <~> onePlusStmts <~> dedent ==> emitSuite
  where newline = ter "NEWLINE"
        indent  = ter "INDENT"
        dedent  = ter "DEDENT"

onePlusStmts :: Parser String
onePlusStmts = stmt <~> zeroPlusStmts ==> emitStmts

zeroPlusStmts :: Parser String
zeroPlusStmts = noMoreStmts
                <|> stmt <~> zeroPlusStmts ==> emitStmts
  where noMoreStmts = eps ""

-- corresponds to `test` in grammar
-- generates all possible conditionals... I think
test :: Parser String
test = orTest
       <|> orTest <~> ifKeyword <~> orTest <~> elseKeyword <~> test ==> emitTest
       <|> lambdef
  where ifKeyword   = ter "if"
        elseKeyword = ter "else"

-- corresponds to `lambdef` in grammar
lambdef :: Parser String
lambdef = lambda <~> zeroOrMoreParams <~> colon <~> body ==> emitLambdef
  where lambda           = ter "lambda"
        zeroOrMoreParams = zeroPlusParams
        colon            = ter ":"
        body             = test

-- corresponds to `or_test` in grammar
orTest :: Parser String
orTest = andTest <~> zeroPlusOrs ==> emitOrTest

zeroPlusOrs :: Parser String
zeroPlusOrs = noMoreTests
              <|> orKeyword <~> andTest <~> zeroPlusOrs ==> emitZeroPlusOrs
  where noMoreTests = eps ""
        orKeyword   = ter "or"

-- corresponds to `and_test` in grammar
andTest :: Parser String
andTest = notTest <~> zeroPlusNots ==> emitAndTest

zeroPlusNots :: Parser String
zeroPlusNots = noMoreNots
               <|> andKeyword <~> notTest <~> zeroPlusNots ==> emitZeroPlusNots
  where noMoreNots = eps ""
        andKeyword = ter "and"

-- corresponds to `not_test` in grammar
notTest :: Parser String
notTest = notKeyword <~> notTest ==> emitNotTest
          <|> comparison
  where notKeyword = ter "not"

-- corresponds to `comparison` in grammar
comparison :: Parser String
comparison = starExpr <~> zeroPlusComps ==> emitComparison

zeroPlusComps :: Parser String
zeroPlusComps = noMoreComps
                <|> comparisonType <~> starExpr <~> zeroPlusComps
                ==> emitZeroPlusComps
  where noMoreComps = eps ""

comparisonType :: Parser String
comparisonType = comparisonOperator ==> emitComparisonOperator
                <|> inKeyword
                <|> is
                <|> (not <~> inKeyword ==> emitNotIn)
                <|> (is <~> not ==> emitIsNot)
  where inKeyword   = ter "in"
        not         = ter "not"
        is          = ter "is"
        emitNotIn _ = "not-in"
        emitIsNot _ = "is-not"

comparisonOperator :: Parser String
comparisonOperator = ter "<"
                     <|> ter ">"
                     <|> ter "=="
                     <|> ter ">="
                     <|> ter "<="
                     <|> ter "<>"
                     <|> ter "!="

-- corresponds to `star_expr` in grammar
-- matches optional star before the expression, eg `myfunc(*cows)`
starExpr :: Parser String
starExpr = maybeHasStar <~> expr ==> emitStarExpr
  where star         = ter "*"
        noStar       = eps ""
        maybeHasStar = noStar <|> star

-- corresponds to `expr` in grammar
expr :: Parser String
expr = xorExpr <~> zeroPlusXors ==> emitExpr

-- corresponds to `xor_expr` in grammar
xorExpr :: Parser String
xorExpr = andExpr <~> zeroPlusAnds ==> emitXorExpr

zeroPlusXors :: Parser String
zeroPlusXors = noMoreXors
               <|> bitOr <~> xorExpr <~> zeroPlusXors ==> emitZeroPlusXors
  where noMoreXors = eps ""
        bitOr  = ter "|"

-- corresponds to `and_expr` in grammar
andExpr :: Parser String
andExpr = shiftExpr <~> zeroPlusShifts ==> emitAndExpr

zeroPlusAnds :: Parser String
zeroPlusAnds = noMoreAnds
               <|> xor <~> andExpr <~> zeroPlusAnds ==> emitZeroPlusAnds
  where noMoreAnds = eps ""
        xor        = ter "^"

-- corresponds to `shift_expr` in grammar
shiftExpr :: Parser String
shiftExpr = arithExpr <~> zeroPlusArithExprs ==> emitShiftExpr

zeroPlusShifts :: Parser String
zeroPlusShifts = noMoreShifts
                 <|> bitAnd <~> shiftExpr <~> zeroPlusShifts
                 ==> emitZeroPlusShifts
  where noMoreShifts = eps ""
        bitAnd       = ter "&"

-- corresponds to `arith_expr` in grammar
arithExpr :: Parser String
arithExpr = term <~> zeroPlusAdds ==> emitArithExpr

emitArithExpr :: (String,String) -> String
emitArithExpr (termExp, restOfAdds) = case restOfAdds of
  [] -> termExp
  _  -> joinStrs [header, body, footer]
  where header = "(arith "
        body   = join " " [termExp, restOfAdds]
        footer = ")"

zeroPlusArithExprs :: Parser String
zeroPlusArithExprs = noMoreArithExprs
                     <|> leftOrRightShift <~> arithExpr <~> zeroPlusArithExprs
                     ==> emitZeroPlusArithExprs
  where noMoreArithExprs = eps ""
        leftOrRightShift = ter "<<" <|> ter ">>"

zeroPlusAdds :: Parser String
zeroPlusAdds = noMoreAdds
               <|> minusOrPlus <~> term <~> zeroPlusAdds ==> emitZeroPlusAdds
  where noMoreAdds  = eps ""
        minusOrPlus = ter "+" <|> ter "-"

-- corresponds to `term` in grammar
term :: Parser String
term = factor <~> zeroPlusMults ==> emitTerm

zeroPlusMults :: Parser String
zeroPlusMults = noMoreMults
                <|> operator <~> factor <~> zeroPlusMults ==> emitZeroPlusMults
  where noMoreMults = eps ""
        mult        = ter "*"
        divis       = ter "/"
        modu        = ter "%"
        intdiv      = ter "//"
        operator    = mult <|> divis <|> modu <|> intdiv

-- corresponds to `factor` in grammar
factor :: Parser String
factor = power
         <|> operatorChoice <~> factor ==> emitFactor
  where plus           = ter "+"
        minus          = ter "-"
        bitNot         = ter "~"
        operatorChoice = plus <|> minus <|> bitNot

indexed :: Parser String
indexed = atom <~> zeroPlusTrailers ==> emitIndexed

-- corresponds to `power` in grammar
power :: Parser String
power = indexed <~> exponent ==> emitPower
  where noPower      = eps ""
        raisedTo     = ter "**"
        emitExponent = (\(_, fctr) -> fctr)
        exponent     = noPower <|> ((raisedTo <~> factor) ==> emitExponent)

-- corresponds to `atom` in grammar
atom :: Parser String
atom = tuple
       <|> list
       <|> dict
       <|> id        ==> emitId
       <|> lit       ==> emitLit
       <|> str       ==> emitString
       <|> dots      ==> emitDots
       <|> noneType  ==> emitNoneType
       <|> trueType  ==> emitTrueType
       <|> falseType ==> emitFalseType
  where id              = ter "ID"
        lit             = ter "LIT"
        dots            = ter "..."
        noneType        = ter "None"
        trueType        = ter "True"
        falseType       = ter "False"
        emitId        x = x
        emitLit       x = x
        emitString    x = joinStrs ["\"", x, "\""]
        emitDots      _ = ""
        emitNoneType  _ = "None"
        emitTrueType  _ = "True"
        emitFalseType _ = "False"

tuple :: Parser String
tuple = lparen <~> optionalExps <~> rparen ==> emitTuple
  where lparen       = ter "("
        rparen       = ter ")"
        none         = eps ""
        optionalExps = (none <|> testOrTests)

list :: Parser String
list = lbracket <~> optionalExps <~> rbracket ==> emitList
  where lbracket     = ter "["
        rbracket     = ter "]"
        none         = eps ""
        optionalExps = none <|> manyTests

dict :: Parser String
dict = lbrace <~> optDictOrSetMaker <~> rbrace ==> emitDict
  where lbrace            = ter "{"
        rbrace            = ter "}"
        none              = eps ""
        optDictOrSetMaker = none <|> dictorsetmaker

str :: Parser String
str = string <~> zeroPlusStrs ==> emitStr
  where string = ter "STRING" 

zeroPlusStrs :: Parser String
zeroPlusStrs = noMoreStrs
               <|> string <~> zeroPlusStrs ==> emitStr
  where noMoreStrs = eps ""
        string     = ter "STRING"

-- corresponds to `trailer` in grammar
trailer :: Parser String
trailer = leftParen <~> optionalArglist <~> rightParen ==> emitTrailerTuple
          <|> subscript ==> emitSubscript
          <|> methodCall ==> emitMethodCall
  where leftParen       = ter "("
        rightParen      = ter ")"
        leftBracket     = ter "["
        rightBracket    = ter "]"
        noArglist       = eps ""
        optionalArglist = noArglist <|> arglist
        subscript       = leftBracket <~> testOrTests <~> rightBracket
        methodCall      = period <~> id
        period          = ter "."
        id              = ter "ID"

zeroPlusTrailers :: Parser String
zeroPlusTrailers = noMoreTrailers
                   <|> trailer <~> zeroPlusTrailers ==> emitZeroPlusTrailers
  where noMoreTrailers = eps ""

manyTests :: Parser String
manyTests = test <~> testTuple <~> optionalComma ==> emitManyTests
  where nothing       = eps ""
        comma         = ter ","
        optionalComma = nothing <|> comma

-- a test exp can either be just one expr or a tuple eg `ex1, ex2 = blah()`
testOrTests :: Parser String
testOrTests = exp <~> zeroPlusExps <~> optionalComma ==> emitTestOrTests
  where exp           = test
        zeroPlusExps  = zeroPlusArgs
        noComma       = eps ""
        comma         = ter ","
        optionalComma = noComma <|> comma

-- corresponds to `dictorsetmaker` in grammar
dictorsetmaker :: Parser String
dictorsetmaker = dictMaker <|> setMaker

dictMaker :: Parser String
dictMaker = test <~> colon <~> test <~> testTuple' <~> optionalComma
            ==> emitDictMaker
  where colon = ter ":"
        nothing = eps ""
        comma = ter ","
        optionalComma = nothing <|> comma

setMaker :: Parser String
setMaker = test <~> testTuple <~> optionalComma ==> emitSetMaker
  where nothing       = eps ""
        comma         = ter ","
        optionalComma = nothing <|> comma

testTuple :: Parser String
testTuple = noMoreTests
            <|> comma <~> test <~> testTuple ==> emitTestTuple
  where noMoreTests = eps ""
        comma = ter ","

testTuple' :: Parser String
testTuple' = nothing
             <|> comma <~> test <~> colon <~> test <~> testTuple'
             ==> emitTestTuple'
  where nothing = eps ""
        comma = ter ","
        colon = ter ":"

-- corresponds to `arglist` in grammar
arglist :: Parser String
arglist = arg <~> zeroPlusArgs <~> optionalComma ==> emitArglist
  where arg           = test
        noComma       = eps ""
        comma         = ter ","
        optionalComma = noComma <|> comma

zeroPlusArgs :: Parser String
zeroPlusArgs = noMoreArgs
               <|> comma <~> arg <~> zeroPlusArgs ==> emitZeroPlusArgs
  where noMoreArgs = eps ""
        comma      = ter ","
        arg        = test


