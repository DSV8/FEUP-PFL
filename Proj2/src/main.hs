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
--Binary arithmetic operations
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
  _ -> error "Invalid types for equality comparison"
run (Le : code, IntVal n1 : IntVal n2 : stack, state) = (code, BoolVal (n1 <= n2) : stack, state)
run (Le : _, stack, state) = error "Invalid types for inequality comparison"
--Logical operations
run (And : code, BoolVal n1 : BoolVal n2 : stack, state) = (code, BoolVal (n1 && n2) : stack, state)
run (Neg : code, BoolVal n : stack, state) = (code, BoolVal (not n) : stack, state)
--Stack & Store operations
run (Push n : code, stack, state) = (code, IntVal n : stack, state)
run (Fetch var : code, stack, state) =
  case lookup var state of
    Just val -> (code, val : stack, state)
    Nothing -> error ("Variable not found:" ++ var)
run (Store var : code, val : stack, state) =
  case lookup var state of
    Just _ -> (code, stack, (var, val) : filter (\(v, _) -> v /= var) state)
    Nothing -> (code, stack, (var, val) : state)
run (Store var : code, [], state) = error ("Empty stack, cannot store value on variable: " ++ var)
run (Noop : code, stack, state) = (code, stack, state)
--Control flow operations
run (Branch c1 c2 : code, BoolVal p : stack, state) =
  if p then (c1 ++ code, stack, state) else (c2 ++ code, stack, state)
run (Branch _ _ : _, _, _) = error "Invalid types, cannot perform branch operation"
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

-- TODO: Define the types Aexp, Bexp, Stm and Program

data Aexp =
  NumConst Integer | VarExp String | AddExp Aexp Aexp | SubExp Aexp Aexp | MultExp Aexp Aexp
  deriving Show

data Bexp =
  BoolConst Bool | EqAExp Aexp Aexp | EqBExp Bexp Bexp | LeExp Aexp Aexp | NegExp Bexp | AndExp Bexp Bexp
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
compB (NegExp b) = compB b ++ [Neg]
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
  | otherwise = error $ "Invalid character: " ++ [c]





-- Parsing for arithmetic expressions
parseVarInt :: [String] -> Maybe (Aexp, [String])
parseVarInt (s : restTokens)
    | all isDigit s    = Just (NumConst (read s), restTokens)
    | isLower (head s) = Just (VarExp s, restTokens)
parseVarInt tokens = Nothing

parseVarIntOrParenExpr :: [String] -> Maybe (Aexp, [String])
parseVarIntOrParenExpr ("(" : restTokens1)
    = case parseSumSubOrProdOrVarIntOrPar restTokens1 of
        Just (expr, ")" : restTokens2) ->
            Just (expr, restTokens2)
        Just _ -> Nothing
        Nothing -> Nothing
parseVarIntOrParenExpr tokens = parseVarInt tokens

parseProdOrVarIntOrPar :: [String] -> Maybe (Aexp, [String])
parseProdOrVarIntOrPar tokens = 
    case parseVarIntOrParenExpr tokens of
        Just (expr, restTokens) -> parseAcc expr restTokens
        Nothing -> Nothing
  where
    parseAcc acc ("*" : restTokens) =
        case parseVarIntOrParenExpr restTokens of
            Just (expr, restTokens1) -> parseAcc (MultExp acc expr) restTokens1
            Nothing -> Nothing
    parseAcc acc restTokens = Just (acc, restTokens)

parseSumSubOrProdOrVarIntOrPar :: [String] -> Maybe (Aexp, [String])
parseSumSubOrProdOrVarIntOrPar tokens = 
    case parseProdOrVarIntOrPar tokens of
        Just (expr, restTokens) -> parseAcc expr restTokens
        Nothing -> Nothing
  where
    parseAcc acc ("+" : restTokens) =
        case parseProdOrVarIntOrPar restTokens of
            Just (expr, restTokens1) -> parseAcc (AddExp acc expr) restTokens1
            Nothing -> Nothing
    parseAcc acc ("-" : restTokens) =
        case parseProdOrVarIntOrPar restTokens of
            Just (expr, restTokens1) -> parseAcc (SubExp acc expr) restTokens1
            Nothing -> Nothing
    parseAcc acc restTokens = Just (acc, restTokens)

parseAexp :: [String] -> Maybe(Aexp, [String])
parseAexp tokens =
    case parseSumSubOrProdOrVarIntOrPar tokens of
        Just (expr, restTokens) -> Just (expr, restTokens)
        _ -> Nothing


-- Parsing for boolean expressions
parseBool :: [String] -> Maybe (Bexp, [String])
parseBool ("True" : restTokens)  = Just (BoolConst True, restTokens)
parseBool ("False" : restTokens) = Just (BoolConst False, restTokens)
parseBool tokens = Nothing

parseBoolOrParenExpr :: [String] -> Maybe (Bexp, [String])
parseBoolOrParenExpr ("(" : restTokens1)
    = case parseAndOrEqBOrNotOrEqAOrLeOrBoolOrPar restTokens1 of
        Just (expr, ")" : restTokens2) ->
            Just (expr, restTokens2)
        Just _ -> Nothing
        Nothing -> Nothing
parseBoolOrParenExpr tokens = parseBool tokens

parseLeOrBoolOrPar :: [String] -> Maybe (Bexp, [String])
parseLeOrBoolOrPar tokens = 
    case parseAexp tokens of
        Just (expr1, "<=" : restTokens1) ->
            case parseAexp restTokens1 of
                Just (expr2, restTokens2) ->
                    Just (LeExp expr1 expr2, restTokens2)
                Nothing -> Nothing
        _ -> parseBoolOrParenExpr tokens

parseEqAOrLeOrBoolOrPar :: [String] -> Maybe (Bexp, [String])
parseEqAOrLeOrBoolOrPar tokens = 
    case parseAexp tokens of
        Just (expr1, "==" : restTokens1) ->
            case parseAexp restTokens1 of
                Just (expr2, restTokens2) ->
                    Just (EqAExp expr1 expr2, restTokens2)
                Nothing -> Nothing
        _ -> parseLeOrBoolOrPar tokens

parseNotOrEqAOrLeOrBoolOrPar :: [String] -> Maybe (Bexp, [String])
parseNotOrEqAOrLeOrBoolOrPar ("not" : restTokens1) = 
    case parseEqAOrLeOrBoolOrPar restTokens1 of
        Just (expr, restTokens2) ->
            Just (NegExp expr, restTokens2)
        result -> result
parseNotOrEqAOrLeOrBoolOrPar tokens = parseEqAOrLeOrBoolOrPar tokens

parseEqBOrNotOrEqAOrLeOrBoolOrPar :: [String] -> Maybe (Bexp, [String])
parseEqBOrNotOrEqAOrLeOrBoolOrPar tokens = 
    case parseNotOrEqAOrLeOrBoolOrPar tokens of
        Just (expr, restTokens) -> parseAcc expr restTokens
        _ -> Nothing
  where
    parseAcc acc ("=" : restTokens) =
        case parseNotOrEqAOrLeOrBoolOrPar restTokens of
            Just (expr, restTokens1) -> parseAcc (EqBExp acc expr) restTokens1
            Nothing -> Nothing
    parseAcc acc restTokens = Just (acc, restTokens)

parseAndOrEqBOrNotOrEqAOrLeOrBoolOrPar :: [String] -> Maybe (Bexp, [String])
parseAndOrEqBOrNotOrEqAOrLeOrBoolOrPar tokens = 
    case parseEqBOrNotOrEqAOrLeOrBoolOrPar tokens of
        Just (expr, restTokens) -> parseAcc expr restTokens
        _ -> Nothing
  where
    parseAcc acc ("and" : restTokens) =
        case parseEqBOrNotOrEqAOrLeOrBoolOrPar restTokens of
            Just (expr, restTokens1) -> parseAcc (AndExp acc expr) restTokens1
            Nothing -> Nothing
    parseAcc acc restTokens = Just (acc, restTokens)


parseBexp :: [String] -> Maybe(Bexp, [String])
parseBexp tokens =
    case parseAndOrEqBOrNotOrEqAOrLeOrBoolOrPar tokens of
        Just (expr, restTokens) -> Just (expr, restTokens)
        _ -> Nothing


-- Parsing for statements
parseAss :: [String] -> Maybe (Stm, [String])
parseAss (var : ":=" : rest) = 
    case parseAexp rest of
        Just (expr, restTokens) -> Just (AssignStm var expr, restTokens)
        _ -> Nothing
parseAss _ = Nothing

parseSeq :: [String] -> Maybe (Stm, [String])
parseSeq ("(" : restTokens1) =
    case parseStm restTokens1 of
        Just (stm1, restTokens2) -> 
            case parseStm restTokens2 of
                Just (stm2, ")" : restTokens3) -> Just (SeqStm stm1 stm2, restTokens3)
                _ -> Nothing
        _ -> Nothing
parseSeq _ = Nothing

parseIf :: [String] -> Maybe (Stm, [String])
parseIf ("if" : rest) =
    case parseBexp rest of
        Just (bexp, "then" : restTokens1) ->
            case parseStm restTokens1 of
                Just (stm1, "else" : restTokens2) ->
                    case parseAss restTokens2 of
                        Just (ass, ";" : restTokens3) -> Just (IfStm bexp stm1 ass, restTokens3)
                        _ -> case parseStm restTokens2 of
                                Just (stm2, ";" : restTokens3) -> Just (IfStm bexp stm1 stm2, restTokens3)
                                _ -> Nothing
                _ -> Nothing
parseIf _ = Nothing

parseWhile :: [String] -> Maybe (Stm, [String])
parseWhile ("while" : rest) =
    case parseBexp rest of
        Just (bexp, "do" : restTokens1) -> 
            case parseAss restTokens1 of
                Just (ass, ";" : restTokens2) -> Just (WhileStm bexp ass, restTokens2)
                _ -> case parseStm restTokens1 of
                        Just (stmt, ";" : restTokens2) -> Just (WhileStm bexp stmt, restTokens2)
                        _ -> Nothing
        _ -> Nothing
parseWhile _ = Nothing

parseStm :: [String] -> Maybe (Stm, [String])
parseStm tokens =
    case parseAss tokens of
        Just (stmt, ";" : restTokens) -> Just (stmt, restTokens)
        _ -> case parseSeq tokens of
            Just (stmt, restTokens) -> Just (stmt, restTokens)
            _ -> case parseIf tokens of
                Just (stmt, restTokens) -> Just (stmt, restTokens)
                _ -> case parseWhile tokens of
                    Just (stmt, restTokens) -> Just (stmt, restTokens)
                    _ -> Nothing


-- General Parsing
buildData :: [String] -> Program
buildData [] = []
buildData tokens =
    case parseStm tokens of
        Just (stmt, restTokens) -> stmt : buildData restTokens
        _ -> error "Invalid tokens"

parse :: String -> Program
parse = buildData . lexer




{-
-- To help you test your parser
testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, store2Str store)
  where (_,stack,store) = run(compile (parse programCode), createEmptyStack, createEmptyStore)
-}

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
