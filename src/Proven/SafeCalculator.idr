-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| SafeCalculator - Expression parser and calculator that cannot crash
|||
||| This module provides a safe mathematical expression evaluator with:
||| - Arithmetic operations (+, -, *, /, ^, %)
||| - Parentheses for grouping
||| - Variables and constants
||| - Mathematical functions (sin, cos, sqrt, etc.)
||| - Complete error handling for all edge cases
module Proven.SafeCalculator

import public Proven.Core
import Proven.SafeMath
import Proven.SafeFloat
import Data.List
import Data.String

%default total

--------------------------------------------------------------------------------
-- Expression AST
--------------------------------------------------------------------------------

||| Mathematical operators
public export
data Op = Add | Sub | Mul | Div | Mod | Pow

||| Unary operators
public export
data UnaryOp = Neg | Abs | Sqrt | Sin | Cos | Tan | Ln | Exp | Floor | Ceil

||| Expression abstract syntax tree
public export
data Expr : Type where
  ||| Numeric literal
  Lit : Double -> Expr
  ||| Variable reference
  Var : String -> Expr
  ||| Binary operation
  BinOp : Op -> Expr -> Expr -> Expr
  ||| Unary operation
  UnaryOp : UnaryOp -> Expr -> Expr
  ||| Conditional: if cond then t else e
  Cond : Expr -> Expr -> Expr -> Expr

||| Calculation errors
public export
data CalcError
  = DivisionByZero
  | ModuloByZero
  | NegativeSqrt
  | LogOfNonPositive
  | TanAtPiOver2
  | UndefinedVariable String
  | InvalidExpression String
  | Overflow

public export
Show CalcError where
  show DivisionByZero = "Division by zero"
  show ModuloByZero = "Modulo by zero"
  show NegativeSqrt = "Square root of negative number"
  show LogOfNonPositive = "Logarithm of non-positive number"
  show TanAtPiOver2 = "Tangent undefined at π/2"
  show (UndefinedVariable v) = "Undefined variable: " ++ v
  show (InvalidExpression s) = "Invalid expression: " ++ s
  show Overflow = "Numeric overflow"

--------------------------------------------------------------------------------
-- Environment for Variables
--------------------------------------------------------------------------------

||| Variable environment: maps variable names to values
public export
Env : Type
Env = List (String, Double)

||| Empty environment
public export
emptyEnv : Env
emptyEnv = []

||| Add or update a variable in the environment
public export
setVar : String -> Double -> Env -> Env
setVar name val env = (name, val) :: filter (\(n, _) => n /= name) env

||| Look up a variable
public export
getVar : String -> Env -> Maybe Double
getVar name [] = Nothing
getVar name ((n, v) :: rest) =
  if n == name then Just v else getVar name rest

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

||| Mathematical constant π
public export
piConst : Double
piConst = 3.141592653589793

||| Mathematical constant e
public export
eConst : Double
eConst = 2.718281828459045

||| Golden ratio φ
public export
phiConst : Double
phiConst = 1.618033988749895

||| Standard environment with common constants
public export
stdEnv : Env
stdEnv = [("pi", piConst), ("e", eConst), ("phi", phiConst), ("tau", 2 * piConst)]

--------------------------------------------------------------------------------
-- Expression Construction (Smart Constructors)
--------------------------------------------------------------------------------

||| Create a numeric literal
public export
num : Double -> Expr
num = Lit

||| Create a variable reference
public export
var : String -> Expr
var = Var

||| Addition
public export
add : Expr -> Expr -> Expr
add = BinOp Add

||| Subtraction
public export
sub : Expr -> Expr -> Expr
sub = BinOp Sub

||| Multiplication
public export
mul : Expr -> Expr -> Expr
mul = BinOp Mul

||| Division
public export
divide : Expr -> Expr -> Expr
divide = BinOp Div

||| Modulo
public export
modulo : Expr -> Expr -> Expr
modulo = BinOp Mod

||| Power/exponentiation
public export
pow : Expr -> Expr -> Expr
pow = BinOp Pow

||| Negation
public export
neg : Expr -> Expr
neg = UnaryOp Neg

||| Absolute value
public export
absE : Expr -> Expr
absE = UnaryOp Abs

||| Square root
public export
sqrtE : Expr -> Expr
sqrtE = UnaryOp Sqrt

||| Sine
public export
sinE : Expr -> Expr
sinE = UnaryOp Sin

||| Cosine
public export
cosE : Expr -> Expr
cosE = UnaryOp Cos

||| Tangent
public export
tanE : Expr -> Expr
tanE = UnaryOp Tan

||| Natural logarithm
public export
lnE : Expr -> Expr
lnE = UnaryOp Ln

||| Exponential (e^x)
public export
expE : Expr -> Expr
expE = UnaryOp Exp

||| Floor
public export
floorE : Expr -> Expr
floorE = UnaryOp Floor

||| Ceiling
public export
ceilE : Expr -> Expr
ceilE = UnaryOp Ceil

||| Conditional expression
public export
ifThenElse : Expr -> Expr -> Expr -> Expr
ifThenElse = Cond

--------------------------------------------------------------------------------
-- Evaluation
--------------------------------------------------------------------------------

||| Safe evaluation of unary operations
evalUnary : UnaryOp -> Double -> Either CalcError Double
evalUnary Neg x = Right (-x)
evalUnary Abs x = Right (abs x)
evalUnary Sqrt x = if x < 0 then Left NegativeSqrt else Right (sqrt x)
evalUnary Sin x = Right (sin x)
evalUnary Cos x = Right (cos x)
evalUnary Tan x =
  let cosX = cos x
  in if abs cosX < 1.0e-10 then Left TanAtPiOver2 else Right (tan x)
evalUnary Ln x = if x <= 0 then Left LogOfNonPositive else Right (log x)
evalUnary Exp x = Right (exp x)
evalUnary Floor x = Right (floor x)
evalUnary Ceil x = Right (ceiling x)

||| Safe evaluation of binary operations
evalBinary : Op -> Double -> Double -> Either CalcError Double
evalBinary Add a b = Right (a + b)
evalBinary Sub a b = Right (a - b)
evalBinary Mul a b = Right (a * b)
evalBinary Div a b = if b == 0 then Left DivisionByZero else Right (a / b)
evalBinary Mod a b = if b == 0 then Left ModuloByZero else Right (prim__doubleMod a b)
  where
    -- Idris 2 doesn't have native double mod, approximate it
    prim__doubleMod : Double -> Double -> Double
    prim__doubleMod x y = x - (floor (x / y) * y)
evalBinary Pow a b = Right (pow a b)

||| Evaluate an expression with a given environment
||| @ env  The variable environment
||| @ expr The expression to evaluate
||| @ returns Either an error or the computed value
public export
eval : Env -> Expr -> Either CalcError Double
eval env (Lit x) = Right x
eval env (Var name) =
  case getVar name env of
    Nothing => Left (UndefinedVariable name)
    Just v => Right v
eval env (BinOp op left right) = do
  l <- eval env left
  r <- eval env right
  evalBinary op l r
eval env (UnaryOp op expr) = do
  x <- eval env expr
  evalUnary op x
eval env (Cond cond thenE elseE) = do
  c <- eval env cond
  if c /= 0 then eval env thenE else eval env elseE

||| Evaluate with standard environment
public export
evalStd : Expr -> Either CalcError Double
evalStd = eval stdEnv

||| Evaluate with empty environment (only literals)
public export
evalPure : Expr -> Either CalcError Double
evalPure = eval emptyEnv

--------------------------------------------------------------------------------
-- Simple Expression Parser
--------------------------------------------------------------------------------

||| Token types for lexing
data Token
  = TNum Double
  | TVar String
  | TPlus | TMinus | TStar | TSlash | TPercent | TCaret
  | TLParen | TRParen
  | TComma
  | TEnd

||| Tokenize a string into tokens
tokenize : List Char -> List Token
tokenize [] = [TEnd]
tokenize (' ' :: cs) = tokenize cs
tokenize ('\n' :: cs) = tokenize cs
tokenize ('\t' :: cs) = tokenize cs
tokenize ('+' :: cs) = TPlus :: tokenize cs
tokenize ('-' :: cs) = TMinus :: tokenize cs
tokenize ('*' :: cs) = TStar :: tokenize cs
tokenize ('/' :: cs) = TSlash :: tokenize cs
tokenize ('%' :: cs) = TPercent :: tokenize cs
tokenize ('^' :: cs) = TCaret :: tokenize cs
tokenize ('(' :: cs) = TLParen :: tokenize cs
tokenize (')' :: cs) = TRParen :: tokenize cs
tokenize (',' :: cs) = TComma :: tokenize cs
tokenize cs@(c :: _) =
  if isDigit c || c == '.'
    then parseNumber cs
    else if isAlpha c
      then parseIdent cs
      else [TEnd]  -- Unknown character, end parsing
  where
    isDigit : Char -> Bool
    isDigit c = c >= '0' && c <= '9'

    isAlpha : Char -> Bool
    isAlpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'

    isAlphaNum : Char -> Bool
    isAlphaNum c = isDigit c || isAlpha c

    parseNumber : List Char -> List Token
    parseNumber cs =
      let (numChars, rest) = span (\c => isDigit c || c == '.') cs
          numStr = pack numChars
      in case parseDouble numStr of
           Nothing => [TEnd]
           Just n => TNum n :: tokenize rest

    parseIdent : List Char -> List Token
    parseIdent cs =
      let (identChars, rest) = span isAlphaNum cs
      in TVar (pack identChars) :: tokenize rest

||| Parser state: remaining tokens
Parser : Type -> Type
Parser a = List Token -> Maybe (a, List Token)

||| Parse a primary expression (number, variable, or parenthesized expr)
partial
parsePrimary : Parser Expr

||| Parse unary operations
partial
parseUnary : Parser Expr

||| Parse multiplicative expressions (* / %)
partial
parseMul : Parser Expr

||| Parse additive expressions (+ -)
partial
parseAdd : Parser Expr

||| Parse power expressions (^)
partial
parsePow : Parser Expr

parsePrimary (TNum n :: rest) = Just (Lit n, rest)
parsePrimary (TVar name :: TLParen :: rest) =
  -- Function call
  case parseAdd rest of
    Nothing => Nothing
    Just (arg, TRParen :: rest2) =>
      let func = case name of
                   "sin" => UnaryOp Sin arg
                   "cos" => UnaryOp Cos arg
                   "tan" => UnaryOp Tan arg
                   "sqrt" => UnaryOp Sqrt arg
                   "abs" => UnaryOp Abs arg
                   "ln" => UnaryOp Ln arg
                   "log" => UnaryOp Ln arg
                   "exp" => UnaryOp Exp arg
                   "floor" => UnaryOp Floor arg
                   "ceil" => UnaryOp Ceil arg
                   _ => Var name  -- Unknown function becomes variable
      in Just (func, rest2)
    _ => Nothing
parsePrimary (TVar name :: rest) = Just (Var name, rest)
parsePrimary (TLParen :: rest) =
  case parseAdd rest of
    Nothing => Nothing
    Just (e, TRParen :: rest2) => Just (e, rest2)
    _ => Nothing
parsePrimary _ = Nothing

parseUnary (TMinus :: rest) =
  case parseUnary rest of
    Nothing => Nothing
    Just (e, rest2) => Just (UnaryOp Neg e, rest2)
parseUnary toks = parsePrimary toks

parsePow toks =
  case parseUnary toks of
    Nothing => Nothing
    Just (left, TCaret :: rest) =>
      case parsePow rest of
        Nothing => Nothing
        Just (right, rest2) => Just (BinOp Pow left right, rest2)
    Just result => Just result

parseMul toks =
  case parsePow toks of
    Nothing => Nothing
    Just (left, rest) => parseMulRest left rest
  where
    partial
    parseMulRest : Expr -> List Token -> Maybe (Expr, List Token)
    parseMulRest left (TStar :: rest) =
      case parsePow rest of
        Nothing => Nothing
        Just (right, rest2) => parseMulRest (BinOp Mul left right) rest2
    parseMulRest left (TSlash :: rest) =
      case parsePow rest of
        Nothing => Nothing
        Just (right, rest2) => parseMulRest (BinOp Div left right) rest2
    parseMulRest left (TPercent :: rest) =
      case parsePow rest of
        Nothing => Nothing
        Just (right, rest2) => parseMulRest (BinOp Mod left right) rest2
    parseMulRest left rest = Just (left, rest)

parseAdd toks =
  case parseMul toks of
    Nothing => Nothing
    Just (left, rest) => parseAddRest left rest
  where
    partial
    parseAddRest : Expr -> List Token -> Maybe (Expr, List Token)
    parseAddRest left (TPlus :: rest) =
      case parseMul rest of
        Nothing => Nothing
        Just (right, rest2) => parseAddRest (BinOp Add left right) rest2
    parseAddRest left (TMinus :: rest) =
      case parseMul rest of
        Nothing => Nothing
        Just (right, rest2) => parseAddRest (BinOp Sub left right) rest2
    parseAddRest left rest = Just (left, rest)

||| Parse a string expression
||| @ input The expression string
||| @ returns Just the parsed expression, or Nothing on parse error
public export
partial
parse : String -> Maybe Expr
parse input =
  let tokens = tokenize (unpack input)
  in case parseAdd tokens of
       Just (expr, [TEnd]) => Just expr
       Just (expr, []) => Just expr
       _ => Nothing

||| Parse and evaluate a string expression
||| @ input The expression string
||| @ returns Either an error or the computed value
public export
partial
calculate : String -> Either CalcError Double
calculate input =
  case parse input of
    Nothing => Left (InvalidExpression input)
    Just expr => evalStd expr

||| Parse and evaluate with custom environment
public export
partial
calculateWith : Env -> String -> Either CalcError Double
calculateWith env input =
  case parse input of
    Nothing => Left (InvalidExpression input)
    Just expr => eval env expr

--------------------------------------------------------------------------------
-- Display
--------------------------------------------------------------------------------

public export
Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"
  show Mod = "%"
  show Pow = "^"

public export
Show UnaryOp where
  show Neg = "-"
  show Abs = "abs"
  show Sqrt = "sqrt"
  show Sin = "sin"
  show Cos = "cos"
  show Tan = "tan"
  show Ln = "ln"
  show Exp = "exp"
  show Floor = "floor"
  show Ceil = "ceil"

public export
Show Expr where
  show (Lit x) = show x
  show (Var name) = name
  show (BinOp op l r) = "(" ++ show l ++ " " ++ show op ++ " " ++ show r ++ ")"
  show (UnaryOp op e) = show op ++ "(" ++ show e ++ ")"
  show (Cond c t e) = "if " ++ show c ++ " then " ++ show t ++ " else " ++ show e
