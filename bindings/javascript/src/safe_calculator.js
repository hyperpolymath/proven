// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath
/**
 * SafeCalculator - Expression evaluator with overflow/division protection.
 *
 * Provides safe arithmetic expression evaluation using shunting-yard algorithm.
 * @module
 */

import { ok, err } from './result.js';

/**
 * Token types.
 * @readonly
 * @enum {string}
 */
const TokenType = Object.freeze({
  NUMBER: 'number',
  OPERATOR: 'operator',
  LEFT_PAREN: 'left_paren',
  RIGHT_PAREN: 'right_paren',
  FUNCTION: 'function',
});

/**
 * Operator precedence and associativity.
 */
const OPERATORS = {
  '+': { precedence: 1, associativity: 'left' },
  '-': { precedence: 1, associativity: 'left' },
  '*': { precedence: 2, associativity: 'left' },
  '/': { precedence: 2, associativity: 'left' },
  '%': { precedence: 2, associativity: 'left' },
  '^': { precedence: 3, associativity: 'right' },
};

/**
 * Supported functions.
 */
const FUNCTIONS = new Set(['sin', 'cos', 'tan', 'sqrt', 'abs', 'log', 'ln', 'exp', 'floor', 'ceil', 'round']);

/**
 * Safe calculator operations.
 */
export class SafeCalculator {
  /**
   * Evaluate a mathematical expression safely.
   *
   * @param {string} expression - Mathematical expression
   * @param {Record<string, number>} [variables={}] - Variable values
   * @returns {{ ok: true, value: number } | { ok: false, error: string }}
   *
   * @example
   * SafeCalculator.evaluate("2 + 3 * 4")  // { ok: true, value: 14 }
   * SafeCalculator.evaluate("x + y", { x: 5, y: 3 })  // { ok: true, value: 8 }
   */
  static evaluate(expression, variables = {}) {
    try {
      // Tokenize
      const tokensResult = SafeCalculator.tokenize(expression, variables);
      if (!tokensResult.ok) {
        return tokensResult;
      }

      // Convert to postfix (Reverse Polish Notation)
      const postfixResult = SafeCalculator.toPostfix(tokensResult.value);
      if (!postfixResult.ok) {
        return postfixResult;
      }

      // Evaluate postfix
      return SafeCalculator.evaluatePostfix(postfixResult.value);
    } catch (error) {
      return err(`Evaluation error: ${error.message}`);
    }
  }

  /**
   * Tokenize an expression.
   *
   * @param {string} expression - Expression string
   * @param {Record<string, number>} variables - Variable values
   * @returns {{ ok: true, value: Array<{type: string, value: any}> } | { ok: false, error: string }}
   */
  static tokenize(expression, variables) {
    const tokens = [];
    let charIndex = 0;
    const expr = expression.replace(/\s+/g, '');

    while (charIndex < expr.length) {
      const char = expr[charIndex];

      // Number (including decimals and negative at start)
      if (/\d/.test(char) || (char === '.' && charIndex + 1 < expr.length && /\d/.test(expr[charIndex + 1]))) {
        let numStr = '';
        while (charIndex < expr.length && (/\d/.test(expr[charIndex]) || expr[charIndex] === '.')) {
          numStr += expr[charIndex];
          charIndex++;
        }
        const num = parseFloat(numStr);
        if (isNaN(num)) {
          return err(`Invalid number: ${numStr}`);
        }
        tokens.push({ type: TokenType.NUMBER, value: num });
        continue;
      }

      // Operator
      if (char in OPERATORS) {
        // Handle unary minus
        if (char === '-' && (tokens.length === 0 || tokens[tokens.length - 1].type === TokenType.LEFT_PAREN)) {
          // Unary minus - read the number
          charIndex++;
          let numStr = '-';
          while (charIndex < expr.length && (/\d/.test(expr[charIndex]) || expr[charIndex] === '.')) {
            numStr += expr[charIndex];
            charIndex++;
          }
          const num = parseFloat(numStr);
          if (isNaN(num)) {
            return err(`Invalid number: ${numStr}`);
          }
          tokens.push({ type: TokenType.NUMBER, value: num });
          continue;
        }
        tokens.push({ type: TokenType.OPERATOR, value: char });
        charIndex++;
        continue;
      }

      // Parentheses
      if (char === '(') {
        tokens.push({ type: TokenType.LEFT_PAREN, value: '(' });
        charIndex++;
        continue;
      }
      if (char === ')') {
        tokens.push({ type: TokenType.RIGHT_PAREN, value: ')' });
        charIndex++;
        continue;
      }

      // Function or variable
      if (/[a-zA-Z_]/.test(char)) {
        let name = '';
        while (charIndex < expr.length && /[a-zA-Z0-9_]/.test(expr[charIndex])) {
          name += expr[charIndex];
          charIndex++;
        }

        // Check if it's a function
        if (FUNCTIONS.has(name.toLowerCase())) {
          tokens.push({ type: TokenType.FUNCTION, value: name.toLowerCase() });
          continue;
        }

        // Check if it's a constant
        if (name === 'pi' || name === 'PI') {
          tokens.push({ type: TokenType.NUMBER, value: Math.PI });
          continue;
        }
        if (name === 'e' || name === 'E') {
          tokens.push({ type: TokenType.NUMBER, value: Math.E });
          continue;
        }

        // Check if it's a variable
        if (name in variables) {
          tokens.push({ type: TokenType.NUMBER, value: variables[name] });
          continue;
        }

        return err(`Unknown identifier: ${name}`);
      }

      return err(`Unexpected character: ${char}`);
    }

    return ok(tokens);
  }

  /**
   * Convert tokens to postfix notation (shunting-yard algorithm).
   *
   * @param {Array<{type: string, value: any}>} tokens - Tokens
   * @returns {{ ok: true, value: Array<{type: string, value: any}> } | { ok: false, error: string }}
   */
  static toPostfix(tokens) {
    const output = [];
    const operatorStack = [];

    for (const token of tokens) {
      switch (token.type) {
        case TokenType.NUMBER:
          output.push(token);
          break;

        case TokenType.FUNCTION:
          operatorStack.push(token);
          break;

        case TokenType.OPERATOR: {
          const op1 = OPERATORS[token.value];
          while (operatorStack.length > 0) {
            const top = operatorStack[operatorStack.length - 1];
            if (top.type === TokenType.FUNCTION) {
              output.push(operatorStack.pop());
              continue;
            }
            if (top.type !== TokenType.OPERATOR) break;
            const op2 = OPERATORS[top.value];
            if ((op1.associativity === 'left' && op1.precedence <= op2.precedence) || op1.precedence < op2.precedence) {
              output.push(operatorStack.pop());
            } else {
              break;
            }
          }
          operatorStack.push(token);
          break;
        }

        case TokenType.LEFT_PAREN:
          operatorStack.push(token);
          break;

        case TokenType.RIGHT_PAREN: {
          while (operatorStack.length > 0 && operatorStack[operatorStack.length - 1].type !== TokenType.LEFT_PAREN) {
            output.push(operatorStack.pop());
          }
          if (operatorStack.length === 0) {
            return err('Mismatched parentheses');
          }
          operatorStack.pop(); // Remove left paren
          // If top is a function, pop it too
          if (operatorStack.length > 0 && operatorStack[operatorStack.length - 1].type === TokenType.FUNCTION) {
            output.push(operatorStack.pop());
          }
          break;
        }
      }
    }

    while (operatorStack.length > 0) {
      const top = operatorStack.pop();
      if (top.type === TokenType.LEFT_PAREN || top.type === TokenType.RIGHT_PAREN) {
        return err('Mismatched parentheses');
      }
      output.push(top);
    }

    return ok(output);
  }

  /**
   * Evaluate postfix expression.
   *
   * @param {Array<{type: string, value: any}>} postfix - Postfix tokens
   * @returns {{ ok: true, value: number } | { ok: false, error: string }}
   */
  static evaluatePostfix(postfix) {
    const stack = [];

    for (const token of postfix) {
      if (token.type === TokenType.NUMBER) {
        stack.push(token.value);
        continue;
      }

      if (token.type === TokenType.FUNCTION) {
        if (stack.length < 1) {
          return err('Insufficient operands for function');
        }
        const arg = stack.pop();
        const result = SafeCalculator.applyFunction(token.value, arg);
        if (!result.ok) {
          return result;
        }
        stack.push(result.value);
        continue;
      }

      if (token.type === TokenType.OPERATOR) {
        if (stack.length < 2) {
          return err('Insufficient operands for operator');
        }
        const rightOperand = stack.pop();
        const leftOperand = stack.pop();
        const result = SafeCalculator.applyOperator(token.value, leftOperand, rightOperand);
        if (!result.ok) {
          return result;
        }
        stack.push(result.value);
        continue;
      }
    }

    if (stack.length !== 1) {
      return err('Invalid expression');
    }

    const finalResult = stack[0];
    if (!Number.isFinite(finalResult)) {
      return err('Result is not finite');
    }

    return ok(finalResult);
  }

  /**
   * Apply an operator.
   *
   * @param {string} op - Operator
   * @param {number} leftOperand - Left operand
   * @param {number} rightOperand - Right operand
   * @returns {{ ok: true, value: number } | { ok: false, error: string }}
   */
  static applyOperator(op, leftOperand, rightOperand) {
    switch (op) {
      case '+':
        return ok(leftOperand + rightOperand);
      case '-':
        return ok(leftOperand - rightOperand);
      case '*':
        return ok(leftOperand * rightOperand);
      case '/':
        if (rightOperand === 0) {
          return err('Division by zero');
        }
        return ok(leftOperand / rightOperand);
      case '%':
        if (rightOperand === 0) {
          return err('Division by zero');
        }
        return ok(leftOperand % rightOperand);
      case '^':
        return ok(Math.pow(leftOperand, rightOperand));
      default:
        return err(`Unknown operator: ${op}`);
    }
  }

  /**
   * Apply a function.
   *
   * @param {string} fn - Function name
   * @param {number} arg - Argument
   * @returns {{ ok: true, value: number } | { ok: false, error: string }}
   */
  static applyFunction(fn, arg) {
    switch (fn) {
      case 'sin':
        return ok(Math.sin(arg));
      case 'cos':
        return ok(Math.cos(arg));
      case 'tan':
        return ok(Math.tan(arg));
      case 'sqrt':
        if (arg < 0) {
          return err('Cannot take square root of negative number');
        }
        return ok(Math.sqrt(arg));
      case 'abs':
        return ok(Math.abs(arg));
      case 'log':
      case 'ln':
        if (arg <= 0) {
          return err('Logarithm undefined for non-positive numbers');
        }
        return ok(Math.log(arg));
      case 'exp':
        return ok(Math.exp(arg));
      case 'floor':
        return ok(Math.floor(arg));
      case 'ceil':
        return ok(Math.ceil(arg));
      case 'round':
        return ok(Math.round(arg));
      default:
        return err(`Unknown function: ${fn}`);
    }
  }

  /**
   * Validate an expression without evaluating.
   *
   * @param {string} expression - Expression to validate
   * @returns {boolean}
   */
  static isValid(expression) {
    const result = SafeCalculator.evaluate(expression);
    return result.ok;
  }
}
