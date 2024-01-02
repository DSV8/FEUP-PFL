import Data.List
import Data.Ord
import Data.Char

-- Part 1

data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst]

data IntOrBool = IntVal Integer | BoolVal Bool

instance Show IntOrBool
  where
    show (IntVal n) = show n
    show (BoolVal b) = show b

type Stack = [IntOrBool]
type State = [(String, IntOrBool)]

createEmptyStack :: Stack
createEmptyStack = []

stack2Str :: Stack -> String
stack2Str stack = intercalate "," (map show stack)

createEmptyState :: State
createEmptyState = []

state2Str :: State -> String
state2Str state = intercalate "," (map pairToStr (sortBy (comparing fst) state))
  where
    pairToStr (var, val) = var ++ "=" ++ show val

run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, state) = ([], stack, state)

--Arithmetic operations
run (Add : code, IntVal n1 : IntVal n2 : stack, state) = (code, IntVal (n1 + n2) : stack, state)
run (Mult : code, IntVal n1 : IntVal n2 : stack, state) = (code, IntVal (n1 * n2) : stack, state)
run (Sub : code, IntVal n1 : IntVal n2 : stack, state) = (code, IntVal (n1 - n2) : stack, state)

--Boolean operations
run (Tru : code, stack, state) = (code, BoolVal True : stack, state)
run (Fals : code, stack, state) = (code, BoolVal False : stack, state)

--Comparison operations
run (Equ : code, n1 : n2 : stack, state) = case (n1, n2) of
  (IntVal x, IntVal y) -> (code, BoolVal (x == y) : stack, state)
  (BoolVal x, BoolVal y) -> (code, BoolVal (x == y) : stack, state)
  _ -> error "Incorrect type while comparing for equality"
run (Le : code, IntVal n1 : IntVal n2 : stack, state) = (code, BoolVal (n1 <= n2) : stack, state)
run (Le : _, stack, state) = error "Incorrect type while comparing for inequality"

--Logical operations
run (And : code, BoolVal n1 : BoolVal n2 : stack, state) = (code, BoolVal (n1 && n2) : stack, state)
run (Neg : code, BoolVal n : stack, state) = (code, BoolVal (not n) : stack, state)

--Stack & Store operations
run (Push n : code, stack, state) = (code, IntVal n : stack, state)
run (Fetch var : code, stack, state) =
  case lookup var state of
    Just val -> (code, val : stack, state)
    Nothing -> error (var ++ " not found")
run (Store var : code, val : stack, state) =
  case lookup var state of
    Just _ -> (code, stack, (var, val) : filter (\(v, _) -> v /= var) state)
    Nothing -> (code, stack, (var, val) : state)
run (Store var : code, [], state) = error (var ++ "cannot be stored due to empty stack")
run (Noop : code, stack, state) = (code, stack, state)

--Control flow operations
run (Branch c1 c2 : code, BoolVal p : stack, state) =
  if p then (c1 ++ code, stack, state) else (c2 ++ code, stack, state)
run (Branch _ _ : _, _, _) = error "Cannot branch due to incorrect types"
run (Loop c1 c2 : code, stack, state) = (c1 ++ [Branch (c2 ++ [Loop c1 c2]) [Noop]], stack, state)

testAssembler :: Code -> (String, String)
testAssembler code = testAssembleRecursive code createEmptyStack createEmptyState
  where
    testAssembleRecursive [] stack state = (stack2Str stack, state2Str state)
    testAssembleRecursive c s st = testAssembleRecursive newCode newStack newState
      where
        (newCode, newStack, newState) = run (c, s, st)

-- Examples:
-- testAssembler [Push 10,Push 4,Push 3,Sub,Mult] == ("-10","")
-- testAssembler [Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"] == ("","a=3,someVar=False,var=True")
-- testAssembler [Fals,Store "var",Fetch "var"] == ("False","var=False")
-- testAssembler [Push (-20),Tru,Fals] == ("False,True,-20","")
-- testAssembler [Push (-20),Tru,Tru,Neg] == ("False,True,-20","")
-- testAssembler [Push (-20),Tru,Tru,Neg,Equ] == ("False,-20","")
-- testAssembler [Push (-20),Push (-21), Le] == ("True","")
-- testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"] == ("","x=4")
-- testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]] == ("","fact=3628800,i=1")
-- If you test:
-- testAssembler [Push 1,Push 2,And]
-- You should get an exception with the string: "Run-time error"
-- If you test:
-- testAssembler [Tru,Tru,Store "y", Fetch "x",Tru]
-- You should get an exception with the string: "Run-time error"

-- Part 2

data Aexp =
  NumConst Integer | VarExp String | AddExp Aexp Aexp | SubExp Aexp Aexp | MultExp Aexp Aexp
  deriving Show

data Bexp =
  BoolConst Bool | EqAExp Aexp Aexp | EqBExp Bexp Bexp | LeExp Aexp Aexp | NotExp Bexp | AndExp Bexp Bexp
  deriving Show

data Stm =
  AssignStm String Aexp | SeqStm Stm Stm | IfStm Bexp Stm Stm | WhileStm Bexp Stm
  deriving Show

type Program = [Stm]

compA :: Aexp -> Code
compA (NumConst n) = [Push n]
compA (VarExp var) = [Fetch var]
compA (AddExp n1 n2) = compA n2 ++ compA n1 ++ [Add]
compA (SubExp n1 n2) = compA n2 ++ compA n1 ++ [Sub]
compA (MultExp n1 n2) = compA n2 ++ compA n1 ++ [Mult]

compB :: Bexp -> Code
compB (BoolConst b) = [if b then Tru else Fals]
compB (EqAExp n1 n2) = compA n2 ++ compA n1 ++ [Equ]
compB (EqBExp b1 b2) = compB b2 ++ compB b1 ++ [Equ]
compB (LeExp n1 n2) = compA n2 ++ compA n1 ++ [Le]
compB (NotExp b) = compB b ++ [Neg]
compB (AndExp b1 b2) = compB b2 ++ compB b1 ++ [And]

compile :: Program -> Code
compile = concatMap compileStm

compileStm :: Stm -> Code
compileStm (AssignStm var exp) = compA exp ++ [Store var]
compileStm (SeqStm s1 s2) = compileStm s1 ++ compileStm s2
compileStm (IfStm p b1 b2) = compB p ++ [Branch (compileStm b1) (compileStm b2)]
compileStm (WhileStm p c) = [Loop (compB p) (compileStm c)]

lexer :: String -> [String]
lexer [] = []
lexer (c : cs)
  | isSpace c = lexer cs
  | isDigit c = let (n, rest) = span isDigit (c : cs) in n : lexer rest
  | isAlpha c = let (var, rest) = span isAlpha (c : cs) in var : lexer rest
  | elem c "+-*();" = [c] : lexer cs
  | c == '=' || c == '<' || c == ':' =
    case cs of
      ('=' : rest) -> [c, '='] : lexer rest
      _ -> [c] : lexer cs
  | otherwise = error ([c] ++ " is not valid a character")

-- Parsing arithmetic expressions
parseNumVar :: [String] -> Maybe (Aexp, [String])
parseNumVar (t : tsRest)
    | all isDigit t = Just (NumConst (read t), tsRest)
    | not (null t) && isLower (head t) = Just (VarExp t, tsRest)
parseNumVar ts = Nothing

parseNumVarParentheses :: [String] -> Maybe (Aexp, [String])
parseNumVarParentheses ("(" : tsRest1)
    = case parseNumVarSumSub tsRest1 of
        Just (exp, ")" : tsRest2) ->
            Just (exp, tsRest2)
        Just _ -> Nothing
        Nothing -> Nothing
parseNumVarParentheses ts = parseNumVar ts

parseNumVarProd :: [String] -> Maybe (Aexp, [String])
parseNumVarProd ts = 
    case parseNumVarParentheses ts of
        Just (exp, tsRest) -> parseAcc exp tsRest
        Nothing -> Nothing
  where
    parseAcc acc ("*" : tsRest) =
        case parseNumVarParentheses tsRest of
            Just (exp, tsRest1) -> parseAcc (MultExp acc exp) tsRest1
            Nothing -> Nothing
    parseAcc acc tsRest = Just (acc, tsRest)

parseNumVarSumSub :: [String] -> Maybe (Aexp, [String])
parseNumVarSumSub ts = 
    case parseNumVarProd ts of
        Just (exp, tsRest) -> parseAcc exp tsRest
        Nothing -> Nothing
  where
    parseAcc acc ("+" : tsRest) =
        case parseNumVarProd tsRest of
            Just (exp, tsRest1) -> parseAcc (AddExp acc exp) tsRest1
            Nothing -> Nothing
    parseAcc acc ("-" : tsRest) =
        case parseNumVarProd tsRest of
            Just (exp, tsRest1) -> parseAcc (SubExp acc exp) tsRest1
            Nothing -> Nothing
    parseAcc acc tsRest = Just (acc, tsRest)

parseAexp :: [String] -> Maybe(Aexp, [String])
parseAexp ts =
    case parseNumVarSumSub ts of
        Just (exp, tsRest) -> Just (exp, tsRest)
        _ -> Nothing

-- Parsing boolean expressions
parseBool :: [String] -> Maybe (Bexp, [String])
parseBool ("True" : tsRest)  = Just (BoolConst True, tsRest)
parseBool ("False" : tsRest) = Just (BoolConst False, tsRest)
parseBool ts = Nothing

parseBoolParentheses :: [String] -> Maybe (Bexp, [String])
parseBoolParentheses ("(" : tsRest1)
    = case parseBoolAnd tsRest1 of
        Just (exp, ")" : tsRest2) ->
            Just (exp, tsRest2)
        Just _ -> Nothing
        Nothing -> Nothing
parseBoolParentheses ts = parseBool ts

parseBoolLeEqA :: [String] -> Maybe (Bexp, [String])
parseBoolLeEqA ts = 
    case parseAexp ts of
        Just (exp1, op : tsRest1) | op `elem` ["<=", "=="] ->
            case parseAexp tsRest1 of
                Just (exp2, tsRest2) ->
                    case op of
                        "<=" -> Just (LeExp exp1 exp2, tsRest2)
                        "==" -> Just (EqAExp exp1 exp2, tsRest2)
                        _    -> Nothing
                Nothing -> Nothing
        _ -> parseBoolParentheses ts

parseBoolNot :: [String] -> Maybe (Bexp, [String])
parseBoolNot ("not" : tsRest1) = 
    case parseBoolLeEqA tsRest1 of
        Just (exp, tsRest2) ->
            Just (NotExp exp, tsRest2)
        solution -> solution
parseBoolNot ts = parseBoolLeEqA ts

parseBoolEqB :: [String] -> Maybe (Bexp, [String])
parseBoolEqB ts = 
    case parseBoolNot ts of
        Just (exp, tsRest) -> parseAcc exp tsRest
        _ -> Nothing
  where
    parseAcc acc ("=" : tsRest) =
        case parseBoolNot tsRest of
            Just (exp, tsRest1) -> parseAcc (EqBExp acc exp) tsRest1
            Nothing -> Nothing
    parseAcc acc tsRest = Just (acc, tsRest)

parseBoolAnd :: [String] -> Maybe (Bexp, [String])
parseBoolAnd ts = 
    case parseBoolEqB ts of
        Just (exp, tsRest) -> parseAcc exp tsRest
        _ -> Nothing
  where
    parseAcc acc ("and" : tsRest) =
        case parseBoolEqB tsRest of
            Just (exp, tsRest1) -> parseAcc (AndExp acc exp) tsRest1
            Nothing -> Nothing
    parseAcc acc tsRest = Just (acc, tsRest)

parseBexp :: [String] -> Maybe(Bexp, [String])
parseBexp ts =
    case parseBoolAnd ts of
        Just (exp, tsRest) -> Just (exp, tsRest)
        _ -> Nothing

-- Parsing statements
parseAssign :: [String] -> Maybe (Stm, [String])
parseAssign (var : ":=" : rest) = 
    case parseAexp rest of
        Just (exp, tsRest) -> Just (AssignStm var exp, tsRest)
        _ -> Nothing
parseAssign _ = Nothing

parseSeq :: [String] -> Maybe (Stm, [String])
parseSeq ("(" : tsRest1) =
    case parseStm tsRest1 of
        Just (stm1, tsRest2) -> 
            case parseStm tsRest2 of
                Just (stm2, ")" : tsRest3) -> Just (SeqStm stm1 stm2, tsRest3)
                _ -> Nothing
        _ -> Nothing
parseSeq _ = Nothing

parseIf :: [String] -> Maybe (Stm, [String])
parseIf ("if" : rest) =
    case parseBexp rest of
        Just (bexp, "then" : tsRest1) ->
            case parseStm tsRest1 of
                Just (stm1, "else" : tsRest2) ->
                    case parseAssign tsRest2 of
                        Just (aexp, ";" : tsRest3) -> Just (IfStm bexp stm1 aexp, tsRest3)
                        _ -> case parseStm tsRest2 of
                                Just (stm2, ";" : tsRest3) -> Just (IfStm bexp stm1 stm2, tsRest3)
                                _ -> Nothing
                _ -> Nothing
parseIf _ = Nothing

parseWhile :: [String] -> Maybe (Stm, [String])
parseWhile ("while" : rest) =
    case parseBexp rest of
        Just (bexp, "do" : tsRest1) -> 
            case parseAssign tsRest1 of
                Just (aexp, ";" : tsRest2) -> Just (WhileStm bexp aexp, tsRest2)
                _ -> case parseStm tsRest1 of
                        Just (stm, ";" : tsRest2) -> Just (WhileStm bexp stm, tsRest2)
                        _ -> Nothing
        _ -> Nothing
parseWhile _ = Nothing

parseStm :: [String] -> Maybe (Stm, [String])
parseStm ts =
    case parseAssign ts of
        Just (stm, ";" : tsRest) -> Just (stm, tsRest)
        _ -> case parseSeq ts of
            Just (stm, tsRest) -> Just (stm, tsRest)
            _ -> case parseIf ts of
                Just (stm, tsRest) -> Just (stm, tsRest)
                _ -> case parseWhile ts of
                    Just (stm, tsRest) -> Just (stm, tsRest)
                    _ -> Nothing

buildData :: [String] -> Program
buildData [] = []
buildData ts =
    case parseStm ts of
        Just (stm, tsRest) -> stm : buildData tsRest
        _ -> error "Token is not valid"

parse :: String -> Program
parse = buildData . lexer

testParser :: String -> (String, String)
testParser programCode = testParserRecursive (compile (parse programCode)) createEmptyStack createEmptyState
  where
    testParserRecursive [] stack state = (stack2Str stack, state2Str state)
    testParserRecursive code stack state = testParserRecursive newCode newStack newState
      where
        (newCode, newStack, newState) = run (code, stack, state)

-- Examples:
-- testParser "x := 5; x := x - 1;" == ("","x=4")
-- testParser "x := 0 - 2;" == ("","x=-2")
-- testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2")
-- testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;);" == ("","x=1")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
-- testParser "x := 44; if x <= 43 then x := 1; else (x := 33; x := x+1;); y := x*2;" == ("","x=34,y=68")
-- testParser "x := 42; if x <= 43 then (x := 33; x := x+1;) else x := 1;" == ("","x=34")
-- testParser "if (1 == 0+1 = 2+1 == 3) then x := 1; else x := 2;" == ("","x=1")
-- testParser "if (1 == 0+1 = (2+1 == 4)) then x := 1; else x := 2;" == ("","x=2")
-- testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
-- testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")