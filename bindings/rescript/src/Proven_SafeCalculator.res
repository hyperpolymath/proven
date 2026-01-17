// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * SafeCalculator - Expression parser and calculator that cannot crash.
 *
 * Provides safe arithmetic operations with proper error handling for
 * division by zero, invalid expressions, and undefined variables.
 */

/** Error types for calculator operations */
type calcError =
  | InvalidExpression
  | DivisionByZero
  | ModuloByZero
  | NegativeSqrt
  | LogOfNonPositive
  | UndefinedVariable(string)
  | ParenthesisMismatch
  | UnknownFunction(string)
  | Overflow

/** Binary operators */
type binOp =
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | Pow

/** Unary operators/functions */
type unaryOp =
  | Neg
  | Abs
  | Sqrt
  | Sin
  | Cos
  | Tan
  | Ln
  | Log10
  | Exp
  | Floor
  | Ceil
  | Round

/** Token types for lexer */
type tokenType =
  | TNumber
  | TIdentifier
  | TPlus
  | TMinus
  | TStar
  | TSlash
  | TPercent
  | TCaret
  | TLParen
  | TRParen
  | TComma
  | TEof

/** Token representation */
type token = {
  tokenType: tokenType,
  text: string,
  number: float,
}

/** Environment for variables */
type environment = {variables: Js.Dict.t<float>}

/** Create a new environment with mathematical constants */
let makeEnvironment = (): environment => {
  let vars = Js.Dict.empty()
  Js.Dict.set(vars, "pi", Js.Math._PI)
  Js.Dict.set(vars, "e", Js.Math._E)
  Js.Dict.set(vars, "tau", 2.0 *. Js.Math._PI)
  Js.Dict.set(vars, "phi", 1.618033988749895) // Golden ratio
  {variables: vars}
}

/** Set a variable in the environment */
let setVariable = (env: environment, name: string, value: float): unit => {
  Js.Dict.set(env.variables, name, value)
}

/** Get a variable from the environment */
let getVariable = (env: environment, name: string): option<float> => {
  Js.Dict.get(env.variables, name)
}

/** Check if character is a digit */
let isDigit = (c: string): bool => {
  let code = Js.String2.charCodeAt(c, 0)->Belt.Float.toInt
  code >= 48 && code <= 57
}

/** Check if character is alphabetic */
let isAlpha = (c: string): bool => {
  let code = Js.String2.charCodeAt(c, 0)->Belt.Float.toInt
  (code >= 65 && code <= 90) || (code >= 97 && code <= 122)
}

/** Check if character is alphanumeric or underscore */
let isAlphaNumeric = (c: string): bool => {
  isAlpha(c) || isDigit(c) || c == "_"
}

/** Tokenizer state */
type tokenizer = {
  input: string,
  mutable pos: int,
}

/** Create a tokenizer */
let makeTokenizer = (input: string): tokenizer => {
  {input: input, pos: 0}
}

/** Skip whitespace in tokenizer */
let skipWhitespace = (t: tokenizer): unit => {
  let rec loop = () => {
    if t.pos < Js.String2.length(t.input) {
      let c = Js.String2.charAt(t.input, t.pos)
      if c == " " || c == "\t" {
        t.pos = t.pos + 1
        loop()
      }
    }
  }
  loop()
}

/** Get the next token */
let rec getNextToken = (t: tokenizer): token => {
  skipWhitespace(t)

  if t.pos >= Js.String2.length(t.input) {
    {tokenType: TEof, text: "", number: 0.0}
  } else {
    let c = Js.String2.charAt(t.input, t.pos)

    // Single character tokens
    let singleCharToken = switch c {
    | "+" => Some(TPlus)
    | "-" => Some(TMinus)
    | "*" => Some(TStar)
    | "/" => Some(TSlash)
    | "%" => Some(TPercent)
    | "^" => Some(TCaret)
    | "(" => Some(TLParen)
    | ")" => Some(TRParen)
    | "," => Some(TComma)
    | _ => None
    }

    switch singleCharToken {
    | Some(tokType) =>
      t.pos = t.pos + 1
      {tokenType: tokType, text: c, number: 0.0}
    | None =>
      // Number
      if isDigit(c) || c == "." {
        let start = t.pos
        while t.pos < Js.String2.length(t.input) && {
          let ch = Js.String2.charAt(t.input, t.pos)
          isDigit(ch) || ch == "."
        } {
          t.pos = t.pos + 1
        }
        let text = Js.String2.substring(t.input, ~from=start, ~to_=t.pos)
        let num = switch Belt.Float.fromString(text) {
        | Some(n) => n
        | None => 0.0
        }
        {tokenType: TNumber, text: text, number: num}
      } else if isAlpha(c) || c == "_" {
        // Identifier
        let start = t.pos
        while t.pos < Js.String2.length(t.input) && isAlphaNumeric(Js.String2.charAt(t.input, t.pos)) {
          t.pos = t.pos + 1
        }
        let text = Js.String2.substring(t.input, ~from=start, ~to_=t.pos)
        {tokenType: TIdentifier, text: text, number: 0.0}
      } else {
        // Unknown character, skip
        t.pos = t.pos + 1
        getNextToken(t)
      }
    }
  }
}

/** Evaluate a unary function */
let evalFunction = (name: string, arg: float): result<float, calcError> => {
  switch name {
  | "sin" => Ok(Js.Math.sin(arg))
  | "cos" => Ok(Js.Math.cos(arg))
  | "tan" => Ok(Js.Math.tan(arg))
  | "sqrt" =>
    if arg < 0.0 {
      Error(NegativeSqrt)
    } else {
      Ok(Js.Math.sqrt(arg))
    }
  | "abs" => Ok(Js.Math.abs_float(arg))
  | "ln" | "log" =>
    if arg <= 0.0 {
      Error(LogOfNonPositive)
    } else {
      Ok(Js.Math.log(arg))
    }
  | "log10" =>
    if arg <= 0.0 {
      Error(LogOfNonPositive)
    } else {
      Ok(Js.Math.log10(arg))
    }
  | "exp" => Ok(Js.Math.exp(arg))
  | "floor" => Ok(Js.Math.floor_float(arg))
  | "ceil" => Ok(Js.Math.ceil_float(arg))
  | "round" => Ok(Js.Math.round(arg))
  | _ => Error(UnknownFunction(name))
  }
}

/** Parser state */
type parser = {
  tokenizer: tokenizer,
  mutable current: token,
  env: environment,
}

/** Create a parser */
let makeParser = (input: string, env: environment): parser => {
  let t = makeTokenizer(input)
  let current = getNextToken(t)
  {tokenizer: t, current: current, env: env}
}

/** Advance to next token */
let advance = (p: parser): unit => {
  p.current = getNextToken(p.tokenizer)
}

/** Forward declarations for recursive descent parser */
let rec parseAddSub = (p: parser): result<float, calcError> => {
  switch parseMulDiv(p) {
  | Error(e) => Error(e)
  | Ok(left) =>
    let result = ref(left)
    let error = ref(None)

    while error.contents == None && (p.current.tokenType == TPlus || p.current.tokenType == TMinus) {
      let op = p.current.tokenType
      advance(p)
      switch parseMulDiv(p) {
      | Error(e) => error := Some(e)
      | Ok(right) =>
        if op == TPlus {
          result := result.contents +. right
        } else {
          result := result.contents -. right
        }
      }
    }

    switch error.contents {
    | Some(e) => Error(e)
    | None => Ok(result.contents)
    }
  }
}

and parseMulDiv = (p: parser): result<float, calcError> => {
  switch parsePower(p) {
  | Error(e) => Error(e)
  | Ok(left) =>
    let result = ref(left)
    let error = ref(None)

    while error.contents == None &&
    (p.current.tokenType == TStar || p.current.tokenType == TSlash || p.current.tokenType == TPercent) {
      let op = p.current.tokenType
      advance(p)
      switch parsePower(p) {
      | Error(e) => error := Some(e)
      | Ok(right) =>
        if op == TStar {
          result := result.contents *. right
        } else if op == TSlash {
          if right == 0.0 {
            error := Some(DivisionByZero)
          } else {
            result := result.contents /. right
          }
        } else {
          if right == 0.0 {
            error := Some(ModuloByZero)
          } else {
            result := mod_float(result.contents, right)
          }
        }
      }
    }

    switch error.contents {
    | Some(e) => Error(e)
    | None => Ok(result.contents)
    }
  }
}

and parsePower = (p: parser): result<float, calcError> => {
  switch parseUnary(p) {
  | Error(e) => Error(e)
  | Ok(base) =>
    if p.current.tokenType == TCaret {
      advance(p)
      switch parsePower(p) {
      // Right associative
      | Error(e) => Error(e)
      | Ok(exp) => Ok(Js.Math.pow_float(~base, ~exp))
      }
    } else {
      Ok(base)
    }
  }
}

and parseUnary = (p: parser): result<float, calcError> => {
  if p.current.tokenType == TMinus {
    advance(p)
    switch parseUnary(p) {
    | Error(e) => Error(e)
    | Ok(val_) => Ok(-.val_)
    }
  } else if p.current.tokenType == TPlus {
    advance(p)
    parseUnary(p)
  } else {
    parsePrimary(p)
  }
}

and parsePrimary = (p: parser): result<float, calcError> => {
  if p.current.tokenType == TNumber {
    let val_ = p.current.number
    advance(p)
    Ok(val_)
  } else if p.current.tokenType == TIdentifier {
    let name = p.current.text
    advance(p)

    // Check for function call
    if p.current.tokenType == TLParen {
      advance(p)
      switch parseAddSub(p) {
      | Error(e) => Error(e)
      | Ok(arg) =>
        if p.current.tokenType != TRParen {
          Error(ParenthesisMismatch)
        } else {
          advance(p)
          evalFunction(name, arg)
        }
      }
    } else {
      // Variable lookup
      switch getVariable(p.env, name) {
      | Some(val_) => Ok(val_)
      | None => Error(UndefinedVariable(name))
      }
    }
  } else if p.current.tokenType == TLParen {
    advance(p)
    switch parseAddSub(p) {
    | Error(e) => Error(e)
    | Ok(result) =>
      if p.current.tokenType != TRParen {
        Error(ParenthesisMismatch)
      } else {
        advance(p)
        Ok(result)
      }
    }
  } else {
    Error(InvalidExpression)
  }
}

/** Calculate an expression string with default environment */
let calculate = (input: string): result<float, calcError> => {
  let env = makeEnvironment()
  let p = makeParser(input, env)
  parseAddSub(p)
}

/** Calculate an expression string with a custom environment */
let calculateWith = (input: string, env: environment): result<float, calcError> => {
  let p = makeParser(input, env)
  parseAddSub(p)
}

/** Safe division */
let div = (a: float, b: float): result<float, calcError> => {
  if b == 0.0 {
    Error(DivisionByZero)
  } else {
    Ok(a /. b)
  }
}

/** Safe modulo */
let safeMod = (a: float, b: float): result<float, calcError> => {
  if b == 0.0 {
    Error(ModuloByZero)
  } else {
    Ok(mod_float(a, b))
  }
}

/** Safe square root */
let sqrt = (x: float): result<float, calcError> => {
  if x < 0.0 {
    Error(NegativeSqrt)
  } else {
    Ok(Js.Math.sqrt(x))
  }
}

/** Safe natural log */
let ln = (x: float): result<float, calcError> => {
  if x <= 0.0 {
    Error(LogOfNonPositive)
  } else {
    Ok(Js.Math.log(x))
  }
}

/** Safe log base 10 */
let log10 = (x: float): result<float, calcError> => {
  if x <= 0.0 {
    Error(LogOfNonPositive)
  } else {
    Ok(Js.Math.log10(x))
  }
}

/** Power function */
let pow = (base: float, exp: float): float => {
  Js.Math.pow_float(~base, ~exp)
}

/** Convert error to human-readable string */
let errorToString = (error: calcError): string => {
  switch error {
  | InvalidExpression => "Invalid expression"
  | DivisionByZero => "Division by zero"
  | ModuloByZero => "Modulo by zero"
  | NegativeSqrt => "Square root of negative number"
  | LogOfNonPositive => "Logarithm of non-positive number"
  | UndefinedVariable(name) => `Undefined variable: ${name}`
  | ParenthesisMismatch => "Parenthesis mismatch"
  | UnknownFunction(name) => `Unknown function: ${name}`
  | Overflow => "Arithmetic overflow"
  }
}
