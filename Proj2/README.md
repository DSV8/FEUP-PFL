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

--Stack operations
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

--Control flow operations
run (Noop : code, stack, state) = (code, stack, state)
run (Branch c1 c2 : code, BoolVal p : stack, state) =
  if p then (c1 ++ code, stack, state) else (c2 ++ code, stack, state)
run (Branch _ _ : _, _, _) = error "Invalid types, cannot perform branch operation"
run (Loop c1 c2 : code, stack, state) = (c1 ++ [Branch (c2 ++ [Loop c1 c2]) [Noop]], stack, state)
```

For `Binary arithmetic operations`, the interpreter checks if the current element of the `code` is either addition, product or subtraction and for each one it will get the 2 first values of the stack and return the rest of the code and the current state with the result of the operation at the top of the stack.

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
  | otherwise = error $ "Invalid character: " ++ [c]
```

It's easier to demonstrate what the function does with an input. When running `lexer` with the string `"if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;"` the result will be:

```hs
[
  "if", "(", "not", "True", "and", "2", "<=", "5", "=", "3", "==", "4", ")", 
  "then", "x", ":=", "1", ";", "else", "y", ":=", "2", ";"
]
```

Therefore, the function will separate all words, digits and symbols, except if 2 symbols together exist, like `<=`, for example.

