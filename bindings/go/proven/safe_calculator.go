// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

package proven

import (
	"fmt"
	"math"
	"strconv"
	"strings"
	"unicode"
)

// CalculatorError represents a calculator error.
type CalculatorError string

const (
	ErrDivisionByZero    CalculatorError = "division by zero"
	ErrOverflow          CalculatorError = "overflow"
	ErrUnderflow         CalculatorError = "underflow"
	ErrInvalidOperation  CalculatorError = "invalid operation"
	ErrInvalidExpression CalculatorError = "invalid expression"
	ErrStackUnderflow    CalculatorError = "stack underflow"
	ErrInvalidNumber     CalculatorError = "invalid number"
	ErrUnknownFunction   CalculatorError = "unknown function"
	ErrDomainError       CalculatorError = "domain error"
)

func (e CalculatorError) Error() string {
	return string(e)
}

// SafeCalculator provides safe arithmetic operations with overflow checking.
type SafeCalculator struct {
	precision    int
	maxValue     float64
	minValue     float64
	lastResult   float64
	errorMode    ErrorMode
	lastError    error
}

// ErrorMode determines how errors are handled.
type ErrorMode int

const (
	ErrorModeReturn ErrorMode = iota // Return errors
	ErrorModePanic                   // Panic on errors
	ErrorModeClamp                   // Clamp to bounds
)

// NewSafeCalculator creates a calculator with default settings.
func NewSafeCalculator() *SafeCalculator {
	return &SafeCalculator{
		precision: 15,
		maxValue:  math.MaxFloat64,
		minValue:  -math.MaxFloat64,
		errorMode: ErrorModeReturn,
	}
}

// SetPrecision sets decimal precision for display.
func (c *SafeCalculator) SetPrecision(precision int) {
	if precision < 0 {
		precision = 0
	}
	if precision > 20 {
		precision = 20
	}
	c.precision = precision
}

// SetBounds sets minimum and maximum allowed values.
func (c *SafeCalculator) SetBounds(min, max float64) {
	c.minValue = min
	c.maxValue = max
}

// SetErrorMode sets the error handling mode.
func (c *SafeCalculator) SetErrorMode(mode ErrorMode) {
	c.errorMode = mode
}

// LastResult returns the last computation result.
func (c *SafeCalculator) LastResult() float64 {
	return c.lastResult
}

// LastError returns the last error, if any.
func (c *SafeCalculator) LastError() error {
	return c.lastError
}

// Add performs safe addition.
func (c *SafeCalculator) Add(a, b float64) (float64, error) {
	result := a + b
	return c.checkResult(result)
}

// Sub performs safe subtraction.
func (c *SafeCalculator) Sub(a, b float64) (float64, error) {
	result := a - b
	return c.checkResult(result)
}

// Mul performs safe multiplication.
func (c *SafeCalculator) Mul(a, b float64) (float64, error) {
	result := a * b
	return c.checkResult(result)
}

// Div performs safe division.
func (c *SafeCalculator) Div(a, b float64) (float64, error) {
	if b == 0 {
		return c.handleError(0, ErrDivisionByZero)
	}
	result := a / b
	return c.checkResult(result)
}

// Mod performs safe modulo.
func (c *SafeCalculator) Mod(a, b float64) (float64, error) {
	if b == 0 {
		return c.handleError(0, ErrDivisionByZero)
	}
	result := math.Mod(a, b)
	return c.checkResult(result)
}

// Pow performs safe exponentiation.
func (c *SafeCalculator) Pow(base, exp float64) (float64, error) {
	result := math.Pow(base, exp)
	return c.checkResult(result)
}

// Sqrt performs safe square root.
func (c *SafeCalculator) Sqrt(value float64) (float64, error) {
	if value < 0 {
		return c.handleError(0, ErrDomainError)
	}
	result := math.Sqrt(value)
	return c.checkResult(result)
}

// Log performs safe natural logarithm.
func (c *SafeCalculator) Log(value float64) (float64, error) {
	if value <= 0 {
		return c.handleError(0, ErrDomainError)
	}
	result := math.Log(value)
	return c.checkResult(result)
}

// Log10 performs safe base-10 logarithm.
func (c *SafeCalculator) Log10(value float64) (float64, error) {
	if value <= 0 {
		return c.handleError(0, ErrDomainError)
	}
	result := math.Log10(value)
	return c.checkResult(result)
}

// Exp performs safe exponential.
func (c *SafeCalculator) Exp(value float64) (float64, error) {
	result := math.Exp(value)
	return c.checkResult(result)
}

// Abs returns absolute value.
func (c *SafeCalculator) Abs(value float64) (float64, error) {
	result := math.Abs(value)
	return c.checkResult(result)
}

// Floor returns floor of value.
func (c *SafeCalculator) Floor(value float64) (float64, error) {
	result := math.Floor(value)
	return c.checkResult(result)
}

// Ceil returns ceiling of value.
func (c *SafeCalculator) Ceil(value float64) (float64, error) {
	result := math.Ceil(value)
	return c.checkResult(result)
}

// Round rounds to nearest integer.
func (c *SafeCalculator) Round(value float64) (float64, error) {
	result := math.Round(value)
	return c.checkResult(result)
}

// Sin returns sine.
func (c *SafeCalculator) Sin(value float64) (float64, error) {
	result := math.Sin(value)
	return c.checkResult(result)
}

// Cos returns cosine.
func (c *SafeCalculator) Cos(value float64) (float64, error) {
	result := math.Cos(value)
	return c.checkResult(result)
}

// Tan returns tangent.
func (c *SafeCalculator) Tan(value float64) (float64, error) {
	result := math.Tan(value)
	return c.checkResult(result)
}

// Asin returns arcsine.
func (c *SafeCalculator) Asin(value float64) (float64, error) {
	if value < -1 || value > 1 {
		return c.handleError(0, ErrDomainError)
	}
	result := math.Asin(value)
	return c.checkResult(result)
}

// Acos returns arccosine.
func (c *SafeCalculator) Acos(value float64) (float64, error) {
	if value < -1 || value > 1 {
		return c.handleError(0, ErrDomainError)
	}
	result := math.Acos(value)
	return c.checkResult(result)
}

// Atan returns arctangent.
func (c *SafeCalculator) Atan(value float64) (float64, error) {
	result := math.Atan(value)
	return c.checkResult(result)
}

func (c *SafeCalculator) checkResult(result float64) (float64, error) {
	if math.IsNaN(result) {
		return c.handleError(0, ErrInvalidOperation)
	}
	if math.IsInf(result, 1) {
		return c.handleError(c.maxValue, ErrOverflow)
	}
	if math.IsInf(result, -1) {
		return c.handleError(c.minValue, ErrUnderflow)
	}
	if result > c.maxValue {
		return c.handleError(c.maxValue, ErrOverflow)
	}
	if result < c.minValue {
		return c.handleError(c.minValue, ErrUnderflow)
	}

	c.lastResult = result
	c.lastError = nil
	return result, nil
}

func (c *SafeCalculator) handleError(fallback float64, err error) (float64, error) {
	c.lastError = err

	switch c.errorMode {
	case ErrorModePanic:
		panic(err)
	case ErrorModeClamp:
		c.lastResult = fallback
		return fallback, nil
	default:
		return 0, err
	}
}

// Evaluate parses and evaluates a mathematical expression.
func (c *SafeCalculator) Evaluate(expression string) (float64, error) {
	parser := newExpressionParser(expression, c)
	return parser.parse()
}

// Format formats a number with the calculator's precision.
func (c *SafeCalculator) Format(value float64) string {
	return strconv.FormatFloat(value, 'f', c.precision, 64)
}

// expressionParser parses mathematical expressions.
type expressionParser struct {
	input      string
	pos        int
	calculator *SafeCalculator
}

func newExpressionParser(input string, calc *SafeCalculator) *expressionParser {
	return &expressionParser{
		input:      strings.TrimSpace(input),
		pos:        0,
		calculator: calc,
	}
}

func (p *expressionParser) parse() (float64, error) {
	result, err := p.parseExpression()
	if err != nil {
		return 0, err
	}
	if p.pos < len(p.input) {
		return 0, ErrInvalidExpression
	}
	return result, nil
}

func (p *expressionParser) parseExpression() (float64, error) {
	return p.parseAddSub()
}

func (p *expressionParser) parseAddSub() (float64, error) {
	left, err := p.parseMulDiv()
	if err != nil {
		return 0, err
	}

	for p.pos < len(p.input) {
		p.skipWhitespace()
		if p.pos >= len(p.input) {
			break
		}

		op := p.input[p.pos]
		if op != '+' && op != '-' {
			break
		}
		p.pos++

		right, err := p.parseMulDiv()
		if err != nil {
			return 0, err
		}

		if op == '+' {
			left, err = p.calculator.Add(left, right)
		} else {
			left, err = p.calculator.Sub(left, right)
		}
		if err != nil {
			return 0, err
		}
	}

	return left, nil
}

func (p *expressionParser) parseMulDiv() (float64, error) {
	left, err := p.parsePower()
	if err != nil {
		return 0, err
	}

	for p.pos < len(p.input) {
		p.skipWhitespace()
		if p.pos >= len(p.input) {
			break
		}

		op := p.input[p.pos]
		if op != '*' && op != '/' && op != '%' {
			break
		}
		p.pos++

		right, err := p.parsePower()
		if err != nil {
			return 0, err
		}

		switch op {
		case '*':
			left, err = p.calculator.Mul(left, right)
		case '/':
			left, err = p.calculator.Div(left, right)
		case '%':
			left, err = p.calculator.Mod(left, right)
		}
		if err != nil {
			return 0, err
		}
	}

	return left, nil
}

func (p *expressionParser) parsePower() (float64, error) {
	left, err := p.parseUnary()
	if err != nil {
		return 0, err
	}

	p.skipWhitespace()
	if p.pos < len(p.input) && p.input[p.pos] == '^' {
		p.pos++
		right, err := p.parsePower() // Right associative
		if err != nil {
			return 0, err
		}
		return p.calculator.Pow(left, right)
	}

	return left, nil
}

func (p *expressionParser) parseUnary() (float64, error) {
	p.skipWhitespace()
	if p.pos < len(p.input) && p.input[p.pos] == '-' {
		p.pos++
		value, err := p.parseUnary()
		if err != nil {
			return 0, err
		}
		return -value, nil
	}
	if p.pos < len(p.input) && p.input[p.pos] == '+' {
		p.pos++
		return p.parseUnary()
	}
	return p.parsePrimary()
}

func (p *expressionParser) parsePrimary() (float64, error) {
	p.skipWhitespace()

	if p.pos >= len(p.input) {
		return 0, ErrInvalidExpression
	}

	// Parentheses
	if p.input[p.pos] == '(' {
		p.pos++
		value, err := p.parseExpression()
		if err != nil {
			return 0, err
		}
		p.skipWhitespace()
		if p.pos >= len(p.input) || p.input[p.pos] != ')' {
			return 0, ErrInvalidExpression
		}
		p.pos++
		return value, nil
	}

	// Function call or constant
	if unicode.IsLetter(rune(p.input[p.pos])) {
		return p.parseFunction()
	}

	// Number
	return p.parseNumber()
}

func (p *expressionParser) parseFunction() (float64, error) {
	start := p.pos
	for p.pos < len(p.input) && (unicode.IsLetter(rune(p.input[p.pos])) || unicode.IsDigit(rune(p.input[p.pos]))) {
		p.pos++
	}
	name := strings.ToLower(p.input[start:p.pos])

	// Constants
	switch name {
	case "pi":
		return math.Pi, nil
	case "e":
		return math.E, nil
	}

	// Functions require parentheses
	p.skipWhitespace()
	if p.pos >= len(p.input) || p.input[p.pos] != '(' {
		return 0, fmt.Errorf("%w: %s", ErrUnknownFunction, name)
	}
	p.pos++

	arg, err := p.parseExpression()
	if err != nil {
		return 0, err
	}

	p.skipWhitespace()
	if p.pos >= len(p.input) || p.input[p.pos] != ')' {
		return 0, ErrInvalidExpression
	}
	p.pos++

	switch name {
	case "sqrt":
		return p.calculator.Sqrt(arg)
	case "sin":
		return p.calculator.Sin(arg)
	case "cos":
		return p.calculator.Cos(arg)
	case "tan":
		return p.calculator.Tan(arg)
	case "asin":
		return p.calculator.Asin(arg)
	case "acos":
		return p.calculator.Acos(arg)
	case "atan":
		return p.calculator.Atan(arg)
	case "log", "ln":
		return p.calculator.Log(arg)
	case "log10":
		return p.calculator.Log10(arg)
	case "exp":
		return p.calculator.Exp(arg)
	case "abs":
		return p.calculator.Abs(arg)
	case "floor":
		return p.calculator.Floor(arg)
	case "ceil":
		return p.calculator.Ceil(arg)
	case "round":
		return p.calculator.Round(arg)
	default:
		return 0, fmt.Errorf("%w: %s", ErrUnknownFunction, name)
	}
}

func (p *expressionParser) parseNumber() (float64, error) {
	p.skipWhitespace()
	start := p.pos

	// Optional sign is handled in parseUnary
	for p.pos < len(p.input) && (unicode.IsDigit(rune(p.input[p.pos])) || p.input[p.pos] == '.') {
		p.pos++
	}

	// Scientific notation
	if p.pos < len(p.input) && (p.input[p.pos] == 'e' || p.input[p.pos] == 'E') {
		p.pos++
		if p.pos < len(p.input) && (p.input[p.pos] == '+' || p.input[p.pos] == '-') {
			p.pos++
		}
		for p.pos < len(p.input) && unicode.IsDigit(rune(p.input[p.pos])) {
			p.pos++
		}
	}

	if start == p.pos {
		return 0, ErrInvalidNumber
	}

	value, err := strconv.ParseFloat(p.input[start:p.pos], 64)
	if err != nil {
		return 0, ErrInvalidNumber
	}

	return value, nil
}

func (p *expressionParser) skipWhitespace() {
	for p.pos < len(p.input) && unicode.IsSpace(rune(p.input[p.pos])) {
		p.pos++
	}
}
