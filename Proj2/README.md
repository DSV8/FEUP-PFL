# 2023/2024 PFL Project 2

<div align="justify"><font size="3">For the 2023/2024 Functional and Logical Programming course, we were asked to 2 tasks in Haskell language.</font></div>

<h2>Group: T03_G04</h2>

<div align="left"><font size="3">This project was done by:

- Gonçalo de Castilho Serra Alves Martins (up202108707)
- Diogo Silveira Viana (up202108803)

Both students had a 50% collaboration in the project.</font></div>

## Part 1

### 1.a 

```hs 
type Stack = [IntOrBool]
```
We decided to define `Stack` as represented above due to the examples that were given to us, that showed only bool or integer values being saved to the stack.

### 1.b

```hs
type State = [(String, IntOrBool)]
```

We decided to define `State` as represented above due to the examples that were given to us, that showed that not only bool or integer values were saved, but also variables (hence the `String`).

### 1.c and 1.d

```hs
createEmptyStack :: Stack
createEmptyStack = []

createEmptyState :: State
createEmptyState = []
```

### 1.e

```hs
stack2Str :: Stack -> String
stack2Str stack = intercalate "," (map show stack)
```

`stack2Str` gets the stack and calls `show` for every member of the list and separates them with a comma.

### 1.f

```hs
state2Str :: State -> String
state2Str state = intercalate "," (map pairToStr (sortBy (comparing fst) state))
  where
    pairToStr (var, val) = var ++ "=" ++ show val
```

`state2Str` sorts the state pairs by the first pair and then calls `pairToStr` that will write the pair to a string in the format `x = 1`. After that it will do the same as `stack2Str` function as explained above.

### 1.g

```hs
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
```

For `Arithmetic operations`, the interpreter checks if the current element of the `code` is either addition, product or subtraction and for each one it will get the 2 first values of the stack and return the rest of the code and the current state with the result of the operation at the top of the stack.

For `Boolean operations`, the interpreter will simply put the boolean value at the top of the stack.

For `Comparison operations`, the interpreter will run the condition with the 2 values at the top of the stack and check if the variable types are comparable. The result of the condition (of type Bool) will be placed at the top of the stack, replacing the 2 values that were previously there.

`Logical operations` will replace the 2 top values of the stack with the result of the `and` operation between them or negate the top value if the operation is negation.

`Stack operations` are pretty straight-forward:
- `Push` will place the value at the top of the stack. 
- `Fetch` will lookup the machine's state for the variable in question and if it is found the value will be pushed to the stack.
- `Store` will search the state for the variable in question and alter it's value to the value at the top of the stack. The function will return an error if it cannot find the variable in the state or if there is no value in the stack to save.

`Control Flow operations`:

- `Noop` will return the current code, stack and state.
- `Branch` will run either code1 or code2 depending on whether the value at the top of the stack is true (has to be a boolean value).
- `Loop`, as instructed in the project specification, will be transformed into 
```hs
c1 ++ [Branch (c2 ++ [Loop c1 c2]) [Noop]]
```

## Part 2

### 2.a

```hs
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
```

`Arithmetic expressions` consist of everything that returns numeric values. So numeric values, variables and Addition, Subtraction and Product all belong to this data.

`Boolean expressions` consist of everything that returns boolean values. Therefore, it consists of boolean values, Integer Equality, Boolean Equality, Less or Equal with numeric values, Negation and, finally, And expressions with boolean values.

`Statements` consist of all else. Assignments, sequential statements, If's and else's and While's all belong to this data.

A `Program` is just a list of statements.

### 2.b

```hs
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
```

`compile` will receive the program that was returned by the `parse` function that will be explained later.

Because a program is a list of statements we need to compile each statement from the list using `concatMap`.

`compileStm` will receive a statement as an input and then check which type of statement (defined in `data`) it is:
- `AssignStm` will always store an Arithmetic Expression in a variable, so we will call `compA` to check which type of Arithmetic Expression we are dealing with and act accordingly.
- `SeqStm` means two (or more) sequential statements so we will call the compileStm function again.
- `IfStm` will do b1 if p is true and b2 otherwise (calling `compB` to check what type of Boolean Expression we are dealing with and act accordingly).
- `WhileStm` calls a loop.

### 2.c

Before parsing the input string, we need to run the string through the `lexer` function and only then can we build the data accordingly.

The `lexer` function consists in separating the string so that we can easily read the code to parse it.

```hs
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
```

It's easier to demonstrate what the function does with an input. When running `lexer` with the string `"if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;"` the result will be:

```hs
[
  "if", "(", "not", "True", "and", "2", "<=", "5", "=", "3", "==", "4", ")", 
  "then", "x", ":=", "1", ";", "else", "y", ":=", "2", ";"
]
```

Therefore, the function will separate all words, digits and symbols, except if 2 symbols together exist, like `<=`, for example.

```hs
buildData :: [String] -> Program
buildData [] = []
buildData ts =
    case parseStm ts of
        Just (stm, tsRest) -> stm : buildData tsRest
        _ -> error "Token is not valid"

parse :: String -> Program
parse = buildData . lexer
```

We start by calling `buildData` that receives the list returned by `lexer`. We send it directly to `parseStm`:

```hs
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
```

The function will check which type of statement comes in the list, first checking if it is an assignment, then a sequence and so on.

```hs
parseAssign :: [String] -> Maybe (Stm, [String])
parseAssign (var : ":=" : rest) = 
    case parseAexp rest of
        Just (exp, tsRest) -> Just (AssignStm var exp, tsRest)
        _ -> Nothing
parseAssign _ = Nothing
```

`parseAssign` will check if there is an assignment symbol after the initial variable. If it is, we will run the `parseAexp` function. If not, we go back to `parseStm` and try `parseSeq`.

```hs
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

```

That string will run through all these functions. 

`parseNumVar` will check if the string starts with a variable or a number.

`parseNumVarParentheses` checks if there is an initial parenthesis in the string and give it priority if it exists. If there isn't it will call `parseNumVar`.

`parseNumVarSumSub` will call the `parseNumVarProd` that tries `parseNumVarParentheses`. Regardless of the existence of parentheses or not, it will call `parseAcc`, that will check if there is a sum, subtraction or product in the string. If there are it will call the same functions for the rest of the string, so that the full arithmetic expression is compiled.

```hs
parseSeq :: [String] -> Maybe (Stm, [String])
parseSeq ("(" : tsRest1) =
    case parseStm tsRest1 of
        Just (stm1, tsRest2) -> 
            case parseStm tsRest2 of
                Just (stm2, ")" : tsRest3) -> Just (SeqStm stm1 stm2, tsRest3)
                _ -> Nothing
        _ -> Nothing
parseSeq _ = Nothing
```

`parseSeq` is relatively simple. It checks for the existence of an initial parenthesis and then calls `parceStm` twice so that both statements in the sequence can be compiled.

```hs
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


parseIf :: [String] -> Maybe (Stm, [String])
parseIf ("if" : rest) =
    case parseBexp rest of
        Just (bexp, "then" : tsRest1) ->
            case parseStm tsRest1 of
                Just (stm1, "else" : tsRest2) ->
                    case parseAssign tsRest2 of
                        Just (ass, ";" : tsRest3) -> Just (IfStm bexp stm1 ass, tsRest3)
                        _ -> case parseStm tsRest2 of
                                Just (stm2, ";" : tsRest3) -> Just (IfStm bexp stm1 stm2, tsRest3)
                                _ -> Nothing
                _ -> Nothing
parseIf _ = Nothing
```

`parseIf` is similar to `parseAssign` but for boolean expressions. It will first call `parseBexp` that will check for the existence of a boolean expression. 

`parseBool` will verify if the head of the string contains `True` or `False`.

`parseBoolParentheses` will verify if there are parentheses on the string, giving it priority if true.

`parseBoolLeEqA` will call `parseAexp` and check for operations like `<=` and `==` for conditions with numbers or variables. 

`parseBoolNot` verifies the string for `not` operations.

`parseBoolEqB` for boolean equality `=`.

And finally, `parseBoolAnd` for `and` operators.

```hs
parseWhile :: [String] -> Maybe (Stm, [String])
parseWhile ("while" : rest) =
    case parseBexp rest of
        Just (bexp, "do" : tsRest1) -> 
            case parseAssign tsRest1 of
                Just (ass, ";" : tsRest2) -> Just (WhileStm bexp ass, tsRest2)
                _ -> case parseStm tsRest1 of
                        Just (stm, ";" : tsRest2) -> Just (WhileStm bexp stm, tsRest2)
                        _ -> Nothing
        _ -> Nothing
parseWhile _ = Nothing
```

Finally, `parseWhile` checks for `while` in the string. It starts by looking for the boolean expression associated with keeping the loop running and then checks for assignments and other statements (like if's and else's) that might occur inside the loop.

### Conclusion of Part 2

Even though we explained the code by the order we were given in the project specification, the program for part 2 itself will run in the opposite way. First, we will use `lexer` to separate the input string and then parse it with the functions above, changing the string to the `data types` created in `2.a`. Afterwards, we will send the parsed string to the `compile` function that will alter the string to the machine language used in part 1 and then run the code in that machine.