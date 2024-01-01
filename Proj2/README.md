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

