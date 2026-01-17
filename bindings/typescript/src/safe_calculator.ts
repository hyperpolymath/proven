// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

export interface Result<T> {
  ok: boolean;
  value?: T;
  error?: string;
}

/**
 * SafeCalculator provides safe arithmetic operations.
 */
export class SafeCalculator {
  /**
   * Safe addition.
   */
  static add(a: number, b: number): Result<number> {
    const result = a + b;
    if (!Number.isFinite(result)) {
      return { ok: false, error: 'Addition overflow' };
    }
    return { ok: true, value: result };
  }

  /**
   * Safe subtraction.
   */
  static sub(a: number, b: number): Result<number> {
    const result = a - b;
    if (!Number.isFinite(result)) {
      return { ok: false, error: 'Subtraction overflow' };
    }
    return { ok: true, value: result };
  }

  /**
   * Safe multiplication.
   */
  static mul(a: number, b: number): Result<number> {
    const result = a * b;
    if (!Number.isFinite(result)) {
      return { ok: false, error: 'Multiplication overflow' };
    }
    return { ok: true, value: result };
  }

  /**
   * Safe division.
   */
  static div(a: number, b: number): Result<number> {
    if (b === 0) {
      return { ok: false, error: 'Division by zero' };
    }
    const result = a / b;
    if (!Number.isFinite(result)) {
      return { ok: false, error: 'Division overflow' };
    }
    return { ok: true, value: result };
  }

  /**
   * Safe modulo.
   */
  static mod(a: number, b: number): Result<number> {
    if (b === 0) {
      return { ok: false, error: 'Modulo by zero' };
    }
    return { ok: true, value: a % b };
  }

  /**
   * Safe power.
   */
  static pow(base: number, exponent: number): Result<number> {
    const result = Math.pow(base, exponent);
    if (!Number.isFinite(result)) {
      return { ok: false, error: 'Power overflow' };
    }
    return { ok: true, value: result };
  }

  /**
   * Safe square root.
   */
  static sqrt(value: number): Result<number> {
    if (value < 0) {
      return { ok: false, error: 'Cannot take square root of negative number' };
    }
    return { ok: true, value: Math.sqrt(value) };
  }

  /**
   * Safe natural logarithm.
   */
  static ln(value: number): Result<number> {
    if (value <= 0) {
      return { ok: false, error: 'Logarithm requires positive input' };
    }
    return { ok: true, value: Math.log(value) };
  }

  /**
   * Safe log base 10.
   */
  static log10(value: number): Result<number> {
    if (value <= 0) {
      return { ok: false, error: 'Logarithm requires positive input' };
    }
    return { ok: true, value: Math.log10(value) };
  }

  /**
   * Safe log with arbitrary base.
   */
  static log(value: number, base: number): Result<number> {
    if (value <= 0) {
      return { ok: false, error: 'Logarithm requires positive input' };
    }
    if (base <= 0 || base === 1) {
      return { ok: false, error: 'Invalid logarithm base' };
    }
    return { ok: true, value: Math.log(value) / Math.log(base) };
  }

  /**
   * Safe factorial (for small integers).
   */
  static factorial(n: number): Result<number> {
    if (!Number.isInteger(n) || n < 0) {
      return { ok: false, error: 'Factorial requires non-negative integer' };
    }
    if (n > 170) {
      return { ok: false, error: 'Factorial overflow (n > 170)' };
    }

    let result = 1;
    for (let i = 2; i <= n; i++) {
      result *= i;
    }
    return { ok: true, value: result };
  }

  /**
   * Safe permutation P(n, r).
   */
  static permutation(n: number, r: number): Result<number> {
    if (!Number.isInteger(n) || !Number.isInteger(r) || n < 0 || r < 0) {
      return { ok: false, error: 'Permutation requires non-negative integers' };
    }
    if (r > n) {
      return { ok: false, error: 'r cannot be greater than n' };
    }

    let result = 1;
    for (let i = n - r + 1; i <= n; i++) {
      result *= i;
      if (!Number.isFinite(result)) {
        return { ok: false, error: 'Permutation overflow' };
      }
    }
    return { ok: true, value: result };
  }

  /**
   * Safe combination C(n, r).
   */
  static combination(n: number, r: number): Result<number> {
    if (!Number.isInteger(n) || !Number.isInteger(r) || n < 0 || r < 0) {
      return { ok: false, error: 'Combination requires non-negative integers' };
    }
    if (r > n) {
      return { ok: false, error: 'r cannot be greater than n' };
    }

    // Use symmetry to reduce computation
    if (r > n - r) {
      r = n - r;
    }

    let result = 1;
    for (let i = 0; i < r; i++) {
      result = (result * (n - i)) / (i + 1);
      if (!Number.isFinite(result)) {
        return { ok: false, error: 'Combination overflow' };
      }
    }
    return { ok: true, value: Math.round(result) };
  }

  /**
   * Safe GCD using Euclidean algorithm.
   */
  static gcd(a: number, b: number): Result<number> {
    if (!Number.isInteger(a) || !Number.isInteger(b)) {
      return { ok: false, error: 'GCD requires integers' };
    }

    a = Math.abs(a);
    b = Math.abs(b);

    while (b !== 0) {
      const temp = b;
      b = a % b;
      a = temp;
    }

    return { ok: true, value: a };
  }

  /**
   * Safe LCM.
   */
  static lcm(a: number, b: number): Result<number> {
    const gcdResult = SafeCalculator.gcd(a, b);
    if (!gcdResult.ok) return gcdResult;

    const result = Math.abs(a * b) / gcdResult.value!;
    if (!Number.isFinite(result)) {
      return { ok: false, error: 'LCM overflow' };
    }
    return { ok: true, value: result };
  }

  /**
   * Check if a number is prime.
   */
  static isPrime(n: number): boolean {
    if (!Number.isInteger(n) || n < 2) return false;
    if (n === 2) return true;
    if (n % 2 === 0) return false;

    const sqrt = Math.sqrt(n);
    for (let i = 3; i <= sqrt; i += 2) {
      if (n % i === 0) return false;
    }
    return true;
  }

  /**
   * Safe absolute value.
   */
  static abs(value: number): number {
    return Math.abs(value);
  }

  /**
   * Safe ceiling.
   */
  static ceil(value: number): number {
    return Math.ceil(value);
  }

  /**
   * Safe floor.
   */
  static floor(value: number): number {
    return Math.floor(value);
  }

  /**
   * Safe round.
   */
  static round(value: number, decimals: number = 0): number {
    const factor = Math.pow(10, decimals);
    return Math.round(value * factor) / factor;
  }

  /**
   * Clamp a value to a range.
   */
  static clamp(value: number, min: number, max: number): number {
    return Math.max(min, Math.min(max, value));
  }

  /**
   * Linear interpolation.
   */
  static lerp(a: number, b: number, t: number): number {
    return a + (b - a) * SafeCalculator.clamp(t, 0, 1);
  }

  /**
   * Map a value from one range to another.
   */
  static map(
    value: number,
    inMin: number,
    inMax: number,
    outMin: number,
    outMax: number
  ): Result<number> {
    if (inMax === inMin) {
      return { ok: false, error: 'Input range cannot be zero' };
    }
    const t = (value - inMin) / (inMax - inMin);
    return { ok: true, value: outMin + (outMax - outMin) * t };
  }
}

/**
 * Token types for expression parsing.
 */
enum TokenType {
  Number,
  Operator,
  LeftParen,
  RightParen,
  Function,
  Comma,
  Constant,
}

interface Token {
  type: TokenType;
  value: string | number;
}

/**
 * ExpressionParser evaluates mathematical expressions safely.
 */
export class ExpressionParser {
  private readonly constants: Map<string, number> = new Map([
    ['pi', Math.PI],
    ['e', Math.E],
    ['phi', (1 + Math.sqrt(5)) / 2],
    ['tau', Math.PI * 2],
  ]);

  private readonly functions: Map<string, (args: number[]) => Result<number>> = new Map([
    ['sin', (args) => ({ ok: true, value: Math.sin(args[0]) })],
    ['cos', (args) => ({ ok: true, value: Math.cos(args[0]) })],
    ['tan', (args) => ({ ok: true, value: Math.tan(args[0]) })],
    ['asin', (args) => {
      if (args[0] < -1 || args[0] > 1) return { ok: false, error: 'asin domain error' };
      return { ok: true, value: Math.asin(args[0]) };
    }],
    ['acos', (args) => {
      if (args[0] < -1 || args[0] > 1) return { ok: false, error: 'acos domain error' };
      return { ok: true, value: Math.acos(args[0]) };
    }],
    ['atan', (args) => ({ ok: true, value: Math.atan(args[0]) })],
    ['sqrt', (args) => SafeCalculator.sqrt(args[0])],
    ['ln', (args) => SafeCalculator.ln(args[0])],
    ['log', (args) => SafeCalculator.log10(args[0])],
    ['abs', (args) => ({ ok: true, value: Math.abs(args[0]) })],
    ['ceil', (args) => ({ ok: true, value: Math.ceil(args[0]) })],
    ['floor', (args) => ({ ok: true, value: Math.floor(args[0]) })],
    ['round', (args) => ({ ok: true, value: Math.round(args[0]) })],
    ['exp', (args) => {
      const result = Math.exp(args[0]);
      if (!Number.isFinite(result)) return { ok: false, error: 'exp overflow' };
      return { ok: true, value: result };
    }],
    ['pow', (args) => SafeCalculator.pow(args[0], args[1])],
    ['min', (args) => ({ ok: true, value: Math.min(...args) })],
    ['max', (args) => ({ ok: true, value: Math.max(...args) })],
  ]);

  private readonly precedence: Map<string, number> = new Map([
    ['+', 1],
    ['-', 1],
    ['*', 2],
    ['/', 2],
    ['%', 2],
    ['^', 3],
  ]);

  /**
   * Register a custom constant.
   */
  addConstant(name: string, value: number): void {
    this.constants.set(name.toLowerCase(), value);
  }

  /**
   * Register a custom function.
   */
  addFunction(name: string, fn: (args: number[]) => Result<number>): void {
    this.functions.set(name.toLowerCase(), fn);
  }

  /**
   * Tokenize an expression.
   */
  private tokenize(expr: string): Result<Token[]> {
    const tokens: Token[] = [];
    let i = 0;
    expr = expr.replace(/\s+/g, '');

    while (i < expr.length) {
      const char = expr[i];

      // Number
      if (/[0-9.]/.test(char)) {
        let numStr = '';
        while (i < expr.length && /[0-9.eE+-]/.test(expr[i])) {
          numStr += expr[i++];
        }
        const num = parseFloat(numStr);
        if (isNaN(num)) {
          return { ok: false, error: `Invalid number: ${numStr}` };
        }
        tokens.push({ type: TokenType.Number, value: num });
        continue;
      }

      // Identifier (function or constant)
      if (/[a-zA-Z_]/.test(char)) {
        let name = '';
        while (i < expr.length && /[a-zA-Z0-9_]/.test(expr[i])) {
          name += expr[i++];
        }
        const lowerName = name.toLowerCase();

        if (this.constants.has(lowerName)) {
          tokens.push({ type: TokenType.Constant, value: lowerName });
        } else if (this.functions.has(lowerName)) {
          tokens.push({ type: TokenType.Function, value: lowerName });
        } else {
          return { ok: false, error: `Unknown identifier: ${name}` };
        }
        continue;
      }

      // Operators and parentheses
      if ('+-*/%^'.includes(char)) {
        tokens.push({ type: TokenType.Operator, value: char });
        i++;
        continue;
      }

      if (char === '(') {
        tokens.push({ type: TokenType.LeftParen, value: char });
        i++;
        continue;
      }

      if (char === ')') {
        tokens.push({ type: TokenType.RightParen, value: char });
        i++;
        continue;
      }

      if (char === ',') {
        tokens.push({ type: TokenType.Comma, value: char });
        i++;
        continue;
      }

      return { ok: false, error: `Unexpected character: ${char}` };
    }

    return { ok: true, value: tokens };
  }

  /**
   * Parse and evaluate an expression.
   */
  evaluate(expr: string): Result<number> {
    const tokenResult = this.tokenize(expr);
    if (!tokenResult.ok) return { ok: false, error: tokenResult.error };

    const tokens = tokenResult.value!;
    if (tokens.length === 0) {
      return { ok: false, error: 'Empty expression' };
    }

    try {
      const result = this.parseExpression(tokens, 0);
      if (!result.ok) return result;

      if (!Number.isFinite(result.value!)) {
        return { ok: false, error: 'Result is not a finite number' };
      }

      return result;
    } catch (e) {
      return { ok: false, error: String(e) };
    }
  }

  private parseExpression(
    tokens: Token[],
    minPrec: number,
    startIdx: number = 0
  ): Result<number> & { nextIdx?: number } {
    let result = this.parsePrimary(tokens, startIdx);
    if (!result.ok) return result;

    let value = result.value!;
    let idx = result.nextIdx!;

    while (idx < tokens.length) {
      const token = tokens[idx];
      if (token.type !== TokenType.Operator) break;

      const prec = this.precedence.get(token.value as string) ?? 0;
      if (prec < minPrec) break;

      idx++;
      const rightResult = this.parseExpression(tokens, prec + 1, idx);
      if (!rightResult.ok) return rightResult;

      idx = rightResult.nextIdx!;

      switch (token.value) {
        case '+': {
          const r = SafeCalculator.add(value, rightResult.value!);
          if (!r.ok) return r;
          value = r.value!;
          break;
        }
        case '-': {
          const r = SafeCalculator.sub(value, rightResult.value!);
          if (!r.ok) return r;
          value = r.value!;
          break;
        }
        case '*': {
          const r = SafeCalculator.mul(value, rightResult.value!);
          if (!r.ok) return r;
          value = r.value!;
          break;
        }
        case '/': {
          const r = SafeCalculator.div(value, rightResult.value!);
          if (!r.ok) return r;
          value = r.value!;
          break;
        }
        case '%': {
          const r = SafeCalculator.mod(value, rightResult.value!);
          if (!r.ok) return r;
          value = r.value!;
          break;
        }
        case '^': {
          const r = SafeCalculator.pow(value, rightResult.value!);
          if (!r.ok) return r;
          value = r.value!;
          break;
        }
      }
    }

    return { ok: true, value, nextIdx: idx };
  }

  private parsePrimary(tokens: Token[], idx: number): Result<number> & { nextIdx?: number } {
    if (idx >= tokens.length) {
      return { ok: false, error: 'Unexpected end of expression' };
    }

    const token = tokens[idx];

    // Unary minus
    if (token.type === TokenType.Operator && token.value === '-') {
      const result = this.parsePrimary(tokens, idx + 1);
      if (!result.ok) return result;
      return { ok: true, value: -result.value!, nextIdx: result.nextIdx };
    }

    // Unary plus
    if (token.type === TokenType.Operator && token.value === '+') {
      return this.parsePrimary(tokens, idx + 1);
    }

    // Number
    if (token.type === TokenType.Number) {
      return { ok: true, value: token.value as number, nextIdx: idx + 1 };
    }

    // Constant
    if (token.type === TokenType.Constant) {
      const value = this.constants.get(token.value as string)!;
      return { ok: true, value, nextIdx: idx + 1 };
    }

    // Function call
    if (token.type === TokenType.Function) {
      const fnName = token.value as string;
      idx++;

      if (idx >= tokens.length || tokens[idx].type !== TokenType.LeftParen) {
        return { ok: false, error: `Expected '(' after function ${fnName}` };
      }
      idx++;

      const args: number[] = [];
      while (idx < tokens.length && tokens[idx].type !== TokenType.RightParen) {
        const argResult = this.parseExpression(tokens, 0, idx);
        if (!argResult.ok) return argResult;
        args.push(argResult.value!);
        idx = argResult.nextIdx!;

        if (idx < tokens.length && tokens[idx].type === TokenType.Comma) {
          idx++;
        }
      }

      if (idx >= tokens.length || tokens[idx].type !== TokenType.RightParen) {
        return { ok: false, error: `Expected ')' after function arguments` };
      }
      idx++;

      const fn = this.functions.get(fnName)!;
      const result = fn(args);
      if (!result.ok) return result;
      return { ok: true, value: result.value!, nextIdx: idx };
    }

    // Parenthesized expression
    if (token.type === TokenType.LeftParen) {
      const result = this.parseExpression(tokens, 0, idx + 1);
      if (!result.ok) return result;

      if (result.nextIdx! >= tokens.length || tokens[result.nextIdx!].type !== TokenType.RightParen) {
        return { ok: false, error: 'Missing closing parenthesis' };
      }

      return { ok: true, value: result.value!, nextIdx: result.nextIdx! + 1 };
    }

    return { ok: false, error: `Unexpected token: ${JSON.stringify(token)}` };
  }
}

export const SafeCalculatorModule = {
  SafeCalculator,
  ExpressionParser,
};
