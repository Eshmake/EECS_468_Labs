

{-
Name: Sina Asheghalishahi
Date Created: 04/28/2025
Program: EECS 468 Assignment 08
Description: Haskell program that parses and evaluates an infix expression, and outputs the numerical result
Input: An infix arithmetic expression.
Output: A numerical result (or error for invalid value)
Collaborators: NA
Other Sources: Class Lecture on Haskell Parsing (i.e. Slides 34-(4-21)); stackoverflow.com; chatgpt.com
-}


-- Data type declaration that defines all of the operators (w/ equality checking)
data Op = Add | Sub | Mul | Div | Mod | Exp deriving (Eq)
--  Each Op contructor is converted to its corresponding string via "show" typeclass
instance Show Op where
    show :: Op -> String
    -- PEMDAS operators
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"
    show Mod = "%"
    show Exp = "**"


-- Data type declaration that defines all tokens that can be parsed (w/ equality checking)
data Token = Num Int | Op Op | LeftParen | RightParen deriving (Eq)
--  Each Token contructor is converted to its corresponding string via "show" typeclass
instance Show Token where
    show :: Token -> String
    -- Int constructor mapping
    show (Num n) = show n
    -- Op constructor mapping
    show (Op o) = show o
    -- parentheses constructor mappings
    show LeftParen = "("
    show RightParen = ")"

-- parseTokens func. defined, which converts infix expression to Token list
-- Input: infix char list
-- Output: Token list
parseTokens :: [Char] -> [Token]
-- base case (i.e. empty list mapped to empty list)
parseTokens [] = []
-- case that handles leading negatives (i.e. converts numeric char w/ neg. sign to negative Int), and then recurses
parseTokens ('-' : val : xs)
    | val >= '0' && val <= '9' = Num (-(castToInt val)) : parseTokens xs
-- for each PEMDAS operator, map given operator string to corresponding Token type
parseTokens ('*' : '*' : xs) = Op Exp : parseTokens xs
parseTokens ('%' : xs) = Op Mod : parseTokens xs
parseTokens ('/' : xs) = Op Div : parseTokens xs
parseTokens ('*' : xs) = Op Mul : parseTokens xs
parseTokens ('-' : xs) = Op Sub : parseTokens xs
parseTokens ('+' : xs) = Op Add : parseTokens xs
parseTokens ('(' : xs) = LeftParen : parseTokens xs
parseTokens (')' : xs) = RightParen : parseTokens xs

-- for each number, map given num string to corresponding Token Num value
parseTokens ('0' : xs) = Num 0 : parseTokens xs
parseTokens ('1' : xs) = Num 1 : parseTokens xs
parseTokens ('2' : xs) = Num 2 : parseTokens xs
parseTokens ('3' : xs) = Num 3 : parseTokens xs
parseTokens ('4' : xs) = Num 4 : parseTokens xs
parseTokens ('5' : xs) = Num 5 : parseTokens xs
parseTokens ('6' : xs) = Num 6 : parseTokens xs
parseTokens ('7' : xs) = Num 7 : parseTokens xs
parseTokens ('8' : xs) = Num 8 : parseTokens xs
parseTokens ('9' : xs) = Num 9 : parseTokens xs

-- skip over spaces
parseTokens (' ' : xs) = parseTokens xs
-- for all other string vals, invoke error
parseTokens xs = error ("Invalid Characters: unrecognized token starting with " ++ xs)

-- castToInt helper func. defined, which casts given Char type to Int
-- Input: Char type
-- Output: casted Int type
castToInt :: Char -> Int
-- pattern matching for all digits
castToInt '0' = 0
castToInt '1' = 1
castToInt '2' = 2
castToInt '3' = 3
castToInt '4' = 4
castToInt '5' = 5
castToInt '6' = 6
castToInt '7' = 7
castToInt '8' = 8
castToInt '9' = 9
-- error case for non-digits
castToInt val = error ("Non-digit character: " ++ [val])


-- parseNumbers func. defined, which groups digit symbols into Num type tokens (while accounting for neg. signs)
-- Input: parsed Token list
-- Output: Token list with grouped Num type symbols
parseNumbers :: [Token] -> [Token]
-- base case (i.e. empty list mapped to empty list)
parseNumbers [] = []

-- For a neg. sign with a left-parenthesis to its left and a number to its right, group it with the number as a neg. value
parseNumbers (LeftParen : Op Sub : Num n : xs) = parseNumbers (Num (-n) : xs)
-- For a neg. sign with an operator to its left and a number to its right, group it with the number as a neg. value
parseNumbers (Op o : Op Sub : Num n : xs) =  parseNumbers (Num (-n) : xs)
-- For a sequence of Num type symbols, invoke combDig function to combine them accordingly
parseNumbers (Num n : xs) = combDig n xs

-- for all other tokens, skip and recurse thru remainder of list
parseNumbers (x : xs) = x : parseNumbers xs


-- combDig helper func. defined, which takes in a number, and attaches it to adj. numbers in a sequence
-- Input: integer number w/ token list
-- Output: token list (containing grouped number)
combDig :: Int -> [Token] -> [Token]
-- for an input involving a number and the remaining list containing an immediate number, append the following number to the right of the first one, and recurse
combDig val (Num n : xs) = combDig (val * 10 + n) xs
-- for an input following the numerical sequence, resume recursing w/ parseNumbers
combDig val xs = Num val : parseNumbers xs


-- shunt func. defined, which performs Shunting Yard Algorithm to convert tokenized infix expression to postfix (i.e. Reverse Polish Notation)
-- Input: stack (initially empty) and tokenized infix list
-- Output: tokenized postfix expression
shunt :: ([Token], [Token]) -> [Token]

-- Base Case: both stack and token list are empty
shunt ([], []) = []

-- Token is number: add it to output
shunt (stk, Num x : xs) = Num x : shunt (stk, xs)

-- Token is operator:
-- while there is an operator o2 at the top of the
-- stack which is not a left parenthesis, o2 has >=
-- precedence than o1 in Token list
-- pop o2 from the stack into the output
-- push o1 onto stack
shunt (stk, Op o1 : xs) =
    -- call popWhile function on Op o2 on top of stack while its prec. is >= prec. of o1
    let (popped, restStk) = popWhile (\(Op o2) -> prec o2 >= prec o1) stk
    -- concatenate popped operator w/ result, and recurse w/ shunt
    in popped ++ shunt (Op o1 : restStk, xs)

-- if token is LeftParen, push it onto stack
shunt(stk, LeftParen : xs) = shunt (LeftParen : stk, xs)

-- if token is RightParen, pop operators from stack until top of stack is LeftParen
shunt (stk, RightParen : xs) = 
    -- run popUntilLeftParen on stack
    let (popped, restStk) = popUntilLeftParen stk
    -- concatenate popped operators to result and recurse w/ shunt
    in popped ++ shunt (restStk, xs)

-- if token list is empty, pop remaining items from the operator stack to output
shunt (stk, []) = 
    -- if there are any leftover parentheses in stack, then call error
    if LeftParen `elem` stk || RightParen `elem` stk
    then error "Error: unmatched parentheses"
    -- otherwise, output stack
    else stk

-- prec. helper func. defined, which defines precedence for operators
-- Input: Op type
-- Output: Int type representing operator precedence
prec :: Op -> Int
-- pattern matching for operator precedences
prec Add = 1
prec Sub = 1
prec Mul = 2
prec Div = 2
prec Mod = 2
prec Exp = 3


-- popUntilLeftParen helper func. defined, which pops operators from stack until LeftParen is found
-- Input: token list (i.e. stack from shunt)
-- Output: tuple containing 2 token lists (i.e. popped and remainder of stack)

-- NOTE: stackoverflow.com used for "let-in" keyword pair
popUntilLeftParen :: [Token] -> ([Token], [Token])
-- for empty stack, return error (i.e. operators popped w/o reaching left parenthesis)
popUntilLeftParen [] = error "Mismatched parentheses"
-- when left parenthesis is reached, return empty list for popped, and remainder of stack
popUntilLeftParen (LeftParen : xs) = ([], xs)
-- case involving operators
popUntilLeftParen (x : xs) = 
    -- recurse until left parenthesis is reached, at which point "x" is prepended to popped list 
    let (popped, rest) = popUntilLeftParen xs
    in (x : popped, rest)

-- popWhile helper func. defined, which pops while a condition holds
-- Input: given token w/ conditional, and operator stack
-- Output: tuple of token lists containing popped and stack-remainder token lists

-- NOTE: chatgpt.com used for "@" operator configuration
popWhile :: (Token -> Bool) -> [Token] -> ([Token], [Token])
-- base case: for empty stack, return empty lists
popWhile _ [] = ([], [])
-- for case involving a conditional containing an Op type from the stack
popWhile cond (x@(Op _) : xs)
    -- if Op type matches given conditional, then recurse w/ the conditional and remainder of stack
  | cond x    = let (popped, rest) = popWhile cond xs
                -- then, prepend x to popped list, which is returned alongside remainder of stack
                in (x : popped, rest)
    -- if x does not satisfy given condition, then stop popping from stack, and return empty popped list and stack w/ given Op type
  | otherwise = ([], x : xs)
-- stop popping if a non-operator is encountered
popWhile _ stk = ([], stk)


-- apply func. defined, which arithmetically applies Op types
-- Input: Op type and 2 operands
-- Output: numerical result of operation
apply :: Op -> Float -> Float -> Float
-- pattern matching for all possible operations
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x / y
-- NOTE: formula for mod imitation from chatgpt.com
apply Mod x y = x - (fromIntegral(floor (x/y)) * y)
apply Exp x y = x ** y

-- rpn func. defined, which evaluates postfix expression
-- Input: a stack and token list in postfix
-- Output: numerical result of expression
rpn :: ([Float], [Token]) -> Float
-- handles error cases for divide by zero and no operand
rpn ((0 : y : stk), (Op Div : xs)) = error ("Invalid Expression: Divide by Zero")
rpn ([], (Op o : xs)) = error ("Invalid Expression: Operator w/o Operands (i.e. " ++ show o ++ ") ")
-- Base Case: token list is empty; stack should have a single Float in it
rpn ([x], []) = x
-- handles errors for missing operands and missing operators
rpn ([], []) = error "Invalid Expression: missing operand"
rpn (_:_:_, []) = error "Invalid Expression: missing operator"
-- recursive case, which pops the operands from the stack, applies the operator, and pushes the result back onto the stack.
    -- each operand from token list is converted to a float, and then pushed onto stack
rpn (stk, (Num n : xs)) = rpn ((fromIntegral n : stk), xs)
    -- each operator in token list is applied to corresponding 2 operands from stack, and result is pushed back onto stack
rpn ((x : y : stk), (Op o : xs)) = rpn ((apply o y x : stk), xs)
    -- for all other inputs, call error
rpn _ = error "Invalid Expression: incorrect postfix sequence"


-- parse func. defined, which maps rpn to simple interface
-- Input: String containing infix
-- Output: numerical result
parse :: String -> Float
-- output set equal to sequence of operations
parse input = 
    -- parseTokens invoked on infix input
    let tokens = parseTokens input
        -- parseNumbers invoked on tokenized list
        grouped = parseNumbers tokens
        -- shunt invoked on grouped tokenized list
        postfix = shunt ([], grouped)
    -- run rpn on empty stack and postfix expression
    in rpn ([], postfix)












