module EmitParseTree where

import Data.String.Utils (join)



-- EMISSION FUNCTIONS
-- Functions designed to take the output of a reduction from Derp lib,
-- destructure it using pattern matching, and output a string that is a like
-- a lispy AST.
--
-- Each is accompanied by an emit function that gives hints about how and why
-- the destructuring is happening. For example, `emitFuncdef` has `emitFuncdef'`
-- as a helper, and it takes `id`, `params`, and `body` as parameters, which
-- tells you a lot about how the destructuring works.

emitProgram :: (String,String) -> String
emitProgram (p, _) = emitProgram' p

emitProgram' :: String -> String
emitProgram' prog = joinStrs [header, prog, footer]
  where header = "(program "
        footer = ")"

emitFuncdef :: (String,(String,(String,(String,String)))) -> String
emitFuncdef (_, (id, (params, (_, body)))) = emitFuncdef' id params body

emitFuncdef' :: String -> String -> String -> String
emitFuncdef' id params body = joinStrs [header, wrappedBody, footer]
  where header      = join " " ["(def (" ++ id, params] ++ ") "
        wrappedBody = "(" ++ body
        footer      = "))"

emitParams :: (String, (String, String)) -> String
emitParams (_, (ps, _)) = ps

emitParamList :: (String, String) -> String
emitParamList (s1, s2) = join " " [s1, s2]

emitRestOfParams :: (String,(String,String)) -> String
emitRestOfParams (_, (name, _)) = name

emitNl :: (String,String) -> String
emitNl (_, ln) = ln

emitLine :: (String,String) -> String
emitLine (sp, exp) = join " " [sp, exp]

emitSmallStmts :: (String, (String, String)) -> String
emitSmallStmts (_, (stmts, end)) = joinStrs [wrappedStmt, end]
  where wrappedStmt = "(" ++ stmts ++ ") "

emitSimpleStmt :: (String, (String, (String, String))) -> String
emitSimpleStmt (st1, (st2, _)) = case (null st2) of
  True  -> "(" ++ st1 ++ ")"
  False -> joinStrs [header, body, footer]
  where header = "(begin (" ++ st1 ++ ") "
        body   = st2
        footer = ")"

emitAssignStmt :: (String, (String, String)) -> String
emitAssignStmt (id, (_, rhs)) = joinStrs [lhs, rhs]
  where lhs = "= (" ++ id ++ ")"

emitExprStmt :: String -> String
emitExprStmt st = joinStrs ["expr ", st]

emitAugAssignStmt :: (String, (String, String)) -> String
emitAugAssignStmt (exp,(aug,rhs)) = joinStrs [operator, lhs, rhs]
  where operator = "\"" ++ aug ++ "\" "
        lhs      = "(" ++ exp ++ ") "

emitDelStmt :: (String,String) -> String
emitDelStmt (_, exp) = joinStrs ["del ", exp]

emitPassStmt :: String -> String
emitPassStmt _ = "pass"

emitBreakStmt :: String -> String
emitBreakStmt _ = "break"

emitContinueStmt :: String -> String
emitContinueStmt _ = "continue"

emitReturnStmt :: (String,String) -> String
emitReturnStmt (_, exp) = join " " ["return", exp]

emitGlobalStmt :: (String,(String,String)) -> String
emitGlobalStmt (_, (x, xs)) = joinStrs [global, exps]
  where global = "global "
        exps   = join " " [x,xs]

emitRestOfIds :: (String,(String,String)) -> String
emitRestOfIds (_, (x, xs)) = join " " [x, xs]

emitNonlocalStmt :: (String,(String,String)) -> String
emitNonlocalStmt (_, (x, xs)) = joinStrs [nonlocal, exps]
  where nonlocal = "nonlocal "
        exps     = join " " [x, xs]

emitRestTests :: (String,String) -> String
emitRestTests (_, tst) = tst

emitAssertStmt :: (String,(String,String)) -> String
emitAssertStmt (_, (exp1, exp2)) = joinStrs [assert, exps]
  where assert = "assert "
        exps   = join " " [exp1,exp2]

emitIfStmt :: (String,(String,(String,(String,(String,String))))) -> String
emitIfStmt (_,(t,(_,(s,(elif,els))))) = (join " " [(join " " [("(cond (" ++ t ++ " (" ++ s ++ "))"), elif]) , els]) ++ ")"

emitElseClause :: (String,(String,String)) -> String
emitElseClause (_, (_, body)) = joinStrs [header, body, footer]
  where header = "(else ("
        footer = "))"

emitElifs :: (String,(String,(String,(String,String)))) -> String
emitElifs (_, (tk, (_, (cond, block)))) = join " " [header, block]
  where wrappedCond = " (" ++ cond ++ ")"
        header      = "(" ++ tk ++ wrappedCond ++ ")"

emitWhileElseClause :: (String,(String,String)) -> String
emitWhileElseClause (_, (_, s)) = "(" ++ s ++ ")"

emitWhileStmt :: (String,(String,(String,(String,(String))))) -> String
emitWhileStmt (_,(t,(_,(s,(block))))) =  body ++ footer
  where header = "(while " ++ t ++ " (" ++ s ++ ")"
        body   = join " " [header, block]
        footer = ")"

emitForElseClause :: (String,(String,String)) -> String
emitForElseClause (_, (_, block)) = "(" ++ block ++ ")"

emitForStmt :: (String,(String,(String,(String,(String,(String,String))))))
               -> String
emitForStmt (_, (id, (_, (tst, (_, (stmts, els)))))) = joinStrs [forOpen,
                                                                 forLoop,
                                                                 forClose]
  where forOpen = "(for "
        forLoop = join " " [id, tst, "(", stmts, ")", els]
        forClose = ")"

emitTryStmt :: (String,(String,(String,String))) -> String
emitTryStmt (_, (_, (exp, except))) = joinStrs [header, exp', except, footer]
  where header = "(try ("
        exp'   = exp ++ ") "
        footer = ")"

emitExceptClauses :: (String,(String,(String,String))) -> String
emitExceptClauses (exc, (_, (exp, bl))) = join " " [clause, bl]
  where clause = "(" ++ exc ++ " (" ++ exp ++ "))"

emitExceptClause :: (String,String) -> String
emitExceptClause (_,bl) = joinStrs [except, bl, footer]
  where except = "(except "
        footer = ")"

emitSuite :: (String,(String,(String,String))) -> String
emitSuite (_, (_, (stm, _))) = joinStrs [suitek, stm]
  where suitek = "suite "

emitStmts :: (String,String) -> String
emitStmts (st1, st2) = join " " [st1,st2]

emitExceptType :: (String,String) -> String
emitExceptType (tst, exps) = join " " [tst, exps]

emitExceptVars :: (String,String) -> String
emitExceptVars (_, v) = v

emitRaiseStmt :: (String,String) -> String
emitRaiseStmt (s1, s2) = join " " [s1, s2]

emitRaiseNewException :: (String,String) -> String
emitRaiseNewException (tp, frm) = join " " [tp, frm]

emitFromStmt :: (String,String) -> String
emitFromStmt (_, fr) = fr

emitLambdef :: (String,(String,(String,String))) -> String
emitLambdef (_, (params, (_, body))) = joinStrs [header, middle, footer]
  where header = "(expr (lambda ("
        middle = params ++ ") (" ++ body
        footer = ")))"

emitStarExpr :: (String,String) -> String
emitStarExpr (star, exp) = case star of
  [] -> exp
  _  -> "(star " ++ exp ++ ")"

emitArglist :: (String,(String,String)) -> String
emitArglist (arg, (restOfArgs, _)) = join " " [arg, restOfArgs]

emitZeroPlusArgs :: (String,(String,String)) -> String
emitZeroPlusArgs (_,(arg, restOfArgs)) = join " " [arg, restOfArgs]

emitFinallyTryBlock :: (String,(String,String)) -> String
emitFinallyTryBlock (_, (_, block)) = "() #f (" ++ block ++ ")"

emitFailIfNotParsed :: String -> String
emitFailIfNotParsed parsed = case parsed of
  [] -> "#f"
  _  -> parsed

emitExceptElseClause :: (String,(String,String)) -> String
emitExceptElseClause (_, (_, block)) = "(" ++ block ++ ")"

emitFinallyCatchBlock :: (String,(String,String)) -> String
emitFinallyCatchBlock (_, (_, block)) = "(" ++ block ++ ")"

emitExceptElseFinally :: (String,(String,(String,(String,(String,String))))) ->
                         String
emitExceptElseFinally (except, (_, (block, ([], (elseBlock, finally))))) =
  joinStrs [exceptBlock, elseBlock, " ", finally]
  where exceptBlock = "((" ++ except ++ " (" ++ block ++ "))) "
emitExceptElseFinally (except, (_, (block, (moreExcepts, (elseBlock,
                                                          finally))))) =
  joinStrs [exceptBlock, moreExcepts, ") ", elseBlock, " ", finally]
  where exceptBlock = "((" ++ except ++ " (" ++ block ++ ")) "

emitTest :: (String,(String,(String,(String,String)))) -> String
emitTest (ortest1, (_, (ortest2, (_, testExp)))) =
  join " " [header, ortest1, ortest2, testExp, footer]
  where header = "(expr (if "
        footer = "))"

emitZeroPlusOrs :: (String,(String,String)) -> String
emitZeroPlusOrs (_, (andTestExp, restOfOrs)) = join " " [andTestExp, restOfOrs]

emitOrTest :: (String,String) -> String
emitOrTest (andTestExp, orExps) = case orExps of
  [] -> andTestExp
  _  -> joinStrs [header, body, footer]
  where header = "(or "
        body   = join " " [andTestExp, orExps]
        footer = ")"

emitAndTest :: (String,String) -> String
emitAndTest (notTestExp, restOfNotTests) = case restOfNotTests of
  [] -> notTestExp
  _  -> joinStrs [header, notTestExp, " ", restOfNotTests, footer]
  where header = "(and "
        footer = ")"

emitZeroPlusNots :: (String,(String,String)) -> String
emitZeroPlusNots (_, (notTestExp, restOfNots)) = join " " [notTestExp,restOfNots]

emitNotTest :: (String,String) -> String
emitNotTest (_, notTestExp) = joinStrs [header, notTestExp, footer]
  where header = "(not "
        footer = ")"

emitComparison :: (String,String) -> String
emitComparison (starExp, restOfComps) = case restOfComps of
  [] -> starExp
  _  -> joinStrs [header, starExp, " ", restOfComps, footer]
  where header = "(comparison "
        footer = ")"

emitZeroPlusComps :: (String,(String,String)) -> String
emitZeroPlusComps (cop,(e,r)) = join " " [("(" ++ cop ++ " " ++ e ++ ")"),r]

emitComparisonOperator :: String -> String
emitComparisonOperator x = joinStrs ["\"", x, "\""]

emitZeroPlusXors :: (String,(String,String)) -> String
emitZeroPlusXors (_, (xorExp, restXors)) = join " " [xorExp, restXors]

emitExpr :: (String,String) -> String
emitExpr (xorExp, restXors) = case restXors of
  [] -> xorExp
  _  -> joinStrs [header, xorExp, " ", restXors, footer]
  where header = "(bitwise-or "
        footer = ")"

emitXorExpr :: (String,String) -> String
emitXorExpr (andExp, restOfAnds) = case restOfAnds of
  [] -> andExp
  _  -> joinStrs [header, andExp, " ", restOfAnds, footer]
  where header = "(bitwise-xor "
        footer = ")"

emitZeroPlusAnds :: (String,(String,String)) -> String
emitZeroPlusAnds (_, (andExp, restOfAnds)) = join " " [andExp, restOfAnds]

emitZeroPlusShifts :: (String,(String,String)) -> String
emitZeroPlusShifts (_,(shiftExp, restOfShifts)) =
  join " " [shiftExp, restOfShifts]

emitAndExpr :: (String,String) -> String
emitAndExpr (shiftExp, restOfShiftExps) = case restOfShiftExps of
  [] -> shiftExp
  _  -> joinStrs [header, shiftExp, " ", restOfShiftExps, footer]
  where header = "(bitwise-and "
        footer = ")"

emitShiftExpr :: (String,String) -> String
emitShiftExpr (arithExp, restOfArithExps) = case restOfArithExps of
  [] -> arithExp
  _  -> joinStrs [header, body, footer]
  where header = "(shift "
        body   = join " " [arithExp, restOfArithExps]
        footer = ")"

emitZeroPlusArithExprs :: (String,(String,String)) -> String
emitZeroPlusArithExprs (shiftOperator, (arithExp, restOfArithExps)) =
  join " " [header, body, footer, restOfArithExps]
  where header = "("
        body   = "\"" ++ shiftOperator ++ "\" " ++ arithExp
        footer = ")"

emitZeroPlusAdds :: (String,(String,String)) -> String
emitZeroPlusAdds (oprator, (exp, restOfAdds)) = join " " [header, body, footer,
                                                          restOfAdds]
  where header = "("
        body   = "\"" ++ oprator ++ "\"" ++ exp
        footer = ")"

emitTerm :: (String,String) -> String
emitTerm (fctr, restOfMults) = case restOfMults of
  [] -> fctr
  _  -> joinStrs [header, body, footer]
  where header = "(term "
        body   = join " " [fctr, restOfMults]
        footer = ")"

emitZeroPlusMults :: (String,(String,String)) -> String
emitZeroPlusMults (operator, (operand, restOfOps)) =
  join " " [header, body, footer, restOfOps]
  where header = "("
        body = "\"" ++ operator ++ "\" " ++ operand
        footer = ")"

emitFactor :: (String,String) -> String
emitFactor (operatorChoice, operand) = joinStrs [header, body, footer]
  where header = "("
        body = "\"" ++ operatorChoice ++ "\" " ++ operand
        footer = ")"

emitIndexed :: (String,String) -> String
emitIndexed (atomExp, restOfExps) = case restOfExps of
  [] -> atomExp
  _  -> joinStrs [header, body, footer]
  where header = "(indexed "
        body   = join " " [atomExp, restOfExps]
        footer = ")"

emitZeroPlusTrailers :: (String,String) -> String
emitZeroPlusTrailers (trlr, restOfTrailers) = join " " [trlr, restOfTrailers]

emitPower :: (String,String) -> String
emitPower (coef, pwr) = case pwr of
  [] -> coef
  _  -> joinStrs [header, body, footer]
  where header = "(power "
        body   = coef ++ " " ++ pwr
        footer = ")"

emitStr :: (String,String) -> String
emitStr (string, restOfStrings) = string ++ restOfStrings

emitTrailerTuple :: (String,(String,String)) -> String
emitTrailerTuple (_, (optArglist, _)) = joinStrs [header, optArglist, footer]
  where header = "(called "
        footer = ")"

emitSubscript :: (String,(String,String)) -> String
emitSubscript (_, (listContents, _)) = "(subscript " ++ listContents ++ ")"

emitMethodCall :: (String,String) -> String
emitMethodCall (_, id) = "(dot " ++ id ++ ")"

emitTestOrTests :: (String,(String,String)) -> String
emitTestOrTests (exp, (restOfExps, _)) = case restOfExps of
  [] -> exp
  _  -> joinStrs [header, body, footer]
  where header = "(tuple "
        body   = exp ++ " " ++ restOfExps
        footer = ")"

emitTuple :: (String,(String,String)) -> String
emitTuple (_,(tupleElems,_)) = tupleElems

emitList :: (String,(String,String)) -> String
emitList (_,(listElems,_)) = "(list " ++ listElems ++ ")"

emitDict :: (String,(String,String)) -> String
emitDict (_, (dictionary, _)) = case (null dictionary) of
  True  -> "(dict)"
  False -> dictionary

emitTestTuple :: (String,(String,String)) -> String
emitTestTuple (_, (testExp, restOfTests)) = join " " [testExp, restOfTests]

emitManyTests :: (String,(String,String)) -> String
emitManyTests (test1, (restOfTests, _)) = join " " [test1, restOfTests]

emitTestTuple' :: (String,(String,(String,(String,String)))) -> String
emitTestTuple' (_, (test1, (_, (test2, restOfTests)))) =
  join " " [header, body, footer, restOfTests]
  where header = "("
        body   = test1 ++ " " ++ test2
        footer = ")"

emitDictMaker :: (String,(String,(String,(String,String)))) -> String
emitDictMaker (key, (_, (val, (restOfDict, _)))) = joinStrs [body, footer]
  where dictLit = "(dict (" ++ key ++ " " ++ val ++ ")"
        body = join " " [dictLit, restOfDict]
        footer = ")"

emitSetMaker :: (String,(String,String)) -> String
emitSetMaker (eleme, (restOfElems, (_))) = joinStrs [body, footer]
  where body = join " " [("(set " ++ eleme), restOfElems]
        footer = ")"





joinStrs :: [String] -> String
joinStrs ss = join "" ss
