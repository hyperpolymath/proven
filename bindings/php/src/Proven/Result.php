<?php
// SPDX-License-Identifier: PMPL-1.0-or-later
// SPDX-FileCopyrightText: 2025 Hyperpolymath

declare(strict_types=1);

namespace Proven;

/**
 * Error types for Proven operations.
 */
enum ErrorType: string
{
    case Overflow = 'overflow';
    case DivisionByZero = 'division_by_zero';
    case OutOfBounds = 'out_of_bounds';
    case InvalidUtf8 = 'invalid_utf8';
    case InvalidFormat = 'invalid_format';
    case ParseError = 'parse_error';
    case ValidationError = 'validation_error';
    case EmptyInput = 'empty_input';
    case PathTraversal = 'path_traversal';
    case InjectionDetected = 'injection_detected';
    case TooLong = 'too_long';
    case OutOfRange = 'out_of_range';
    case InvalidInput = 'invalid_input';
}

/**
 * An error from a Proven operation.
 */
readonly class ProvenError
{
    public function __construct(
        public ErrorType $type,
        public string $message,
    ) {}

    /**
     * Create an overflow error.
     */
    public static function overflow(string $message): self
    {
        return new self(ErrorType::Overflow, $message);
    }

    /**
     * Create a division by zero error.
     */
    public static function divisionByZero(): self
    {
        return new self(ErrorType::DivisionByZero, 'Division by zero');
    }

    /**
     * Create an out of bounds error.
     */
    public static function outOfBounds(int $value, int $min, int $max): self
    {
        return new self(
            ErrorType::OutOfBounds,
            sprintf('Value %d out of bounds [%d, %d]', $value, $min, $max)
        );
    }

    /**
     * Create an invalid format error.
     */
    public static function invalidFormat(string $message): self
    {
        return new self(ErrorType::InvalidFormat, $message);
    }

    /**
     * Create a parse error.
     */
    public static function parseError(string $message): self
    {
        return new self(ErrorType::ParseError, $message);
    }

    /**
     * Create a validation error.
     */
    public static function validationError(string $message): self
    {
        return new self(ErrorType::ValidationError, $message);
    }

    /**
     * Create an empty input error.
     */
    public static function emptyInput(): self
    {
        return new self(ErrorType::EmptyInput, 'Empty input not allowed');
    }

    /**
     * Create an out of range error.
     */
    public static function outOfRange(string $message): self
    {
        return new self(ErrorType::OutOfRange, $message);
    }

    /**
     * Create an invalid input error.
     */
    public static function invalidInput(string $message): self
    {
        return new self(ErrorType::InvalidInput, $message);
    }

    public function __toString(): string
    {
        return sprintf('[%s] %s', $this->type->value, $this->message);
    }
}

/**
 * A Result type that represents either success (Ok) or failure (Err).
 *
 * This is a PHP implementation of Rust's Result<T, E> type.
 *
 * @template T The success value type
 */
readonly class Result
{
    private function __construct(
        private bool $isOk,
        private mixed $value,
        private ?ProvenError $error,
    ) {}

    /**
     * Create a successful result.
     *
     * @template V
     * @param V $value The success value
     * @return Result<V>
     */
    public static function ok(mixed $value): self
    {
        return new self(true, $value, null);
    }

    /**
     * Create a failure result.
     *
     * @return Result<never>
     */
    public static function err(ProvenError $error): self
    {
        return new self(false, null, $error);
    }

    /**
     * Check if this is a success.
     */
    public function isOk(): bool
    {
        return $this->isOk;
    }

    /**
     * Check if this is an error.
     */
    public function isErr(): bool
    {
        return !$this->isOk;
    }

    /**
     * Get the success value, or null if error.
     *
     * @return T|null
     */
    public function ok(): mixed
    {
        return $this->isOk ? $this->value : null;
    }

    /**
     * Get the error, or null if success.
     */
    public function err(): ?ProvenError
    {
        return $this->isOk ? null : $this->error;
    }

    /**
     * Get the success value, or throw if error.
     *
     * @return T
     * @throws \RuntimeException
     */
    public function unwrap(): mixed
    {
        if (!$this->isOk) {
            throw new \RuntimeException('Called unwrap() on Err: ' . $this->error);
        }
        return $this->value;
    }

    /**
     * Get the success value, or return the default.
     *
     * @template D
     * @param D $default
     * @return T|D
     */
    public function unwrapOr(mixed $default): mixed
    {
        return $this->isOk ? $this->value : $default;
    }

    /**
     * Get the success value, or call the fallback function.
     *
     * @template D
     * @param callable(ProvenError): D $fn
     * @return T|D
     */
    public function unwrapOrElse(callable $fn): mixed
    {
        return $this->isOk ? $this->value : $fn($this->error);
    }

    /**
     * Get the error value, or throw if success.
     *
     * @throws \RuntimeException
     */
    public function unwrapErr(): ProvenError
    {
        if ($this->isOk) {
            throw new \RuntimeException('Called unwrapErr() on Ok');
        }
        return $this->error;
    }

    /**
     * Map the success value.
     *
     * @template U
     * @param callable(T): U $fn
     * @return Result<U>
     */
    public function map(callable $fn): self
    {
        if ($this->isOk) {
            return self::ok($fn($this->value));
        }
        return $this;
    }

    /**
     * Map the error value.
     *
     * @param callable(ProvenError): ProvenError $fn
     * @return Result<T>
     */
    public function mapErr(callable $fn): self
    {
        if (!$this->isOk) {
            return self::err($fn($this->error));
        }
        return $this;
    }

    /**
     * Chain another operation that returns a Result.
     *
     * @template U
     * @param callable(T): Result<U> $fn
     * @return Result<U>
     */
    public function andThen(callable $fn): self
    {
        if ($this->isOk) {
            return $fn($this->value);
        }
        return $this;
    }

    /**
     * Return the other result if this is Ok, otherwise return this error.
     *
     * @template U
     * @param Result<U> $result
     * @return Result<U>
     */
    public function and(self $result): self
    {
        if ($this->isOk) {
            return $result;
        }
        return $this;
    }

    /**
     * Return this result if Ok, otherwise return the other result.
     *
     * @param Result<T> $result
     * @return Result<T>
     */
    public function or(self $result): self
    {
        if ($this->isOk) {
            return $this;
        }
        return $result;
    }

    /**
     * Check if Ok and the value matches a predicate.
     *
     * @param callable(T): bool $predicate
     */
    public function isOkAnd(callable $predicate): bool
    {
        return $this->isOk && $predicate($this->value);
    }

    /**
     * Check if Err and the error matches a predicate.
     *
     * @param callable(ProvenError): bool $predicate
     */
    public function isErrAnd(callable $predicate): bool
    {
        return !$this->isOk && $predicate($this->error);
    }
}

/**
 * An Option type that represents either Some value or None.
 *
 * This is a PHP implementation of Rust's Option<T> type.
 *
 * @template T The value type
 */
readonly class Option
{
    private function __construct(
        private bool $isSome,
        private mixed $value,
    ) {}

    /**
     * Create a Some option with a value.
     *
     * @template V
     * @param V $value
     * @return Option<V>
     */
    public static function some(mixed $value): self
    {
        return new self(true, $value);
    }

    /**
     * Create a None option.
     *
     * @return Option<never>
     */
    public static function none(): self
    {
        return new self(false, null);
    }

    /**
     * Create an Option from a nullable value.
     *
     * @template V
     * @param V|null $value
     * @return Option<V>
     */
    public static function fromNullable(mixed $value): self
    {
        return $value === null ? self::none() : self::some($value);
    }

    /**
     * Check if this contains a value.
     */
    public function isSome(): bool
    {
        return $this->isSome;
    }

    /**
     * Check if this is empty.
     */
    public function isNone(): bool
    {
        return !$this->isSome;
    }

    /**
     * Get the value, or null if None.
     *
     * @return T|null
     */
    public function get(): mixed
    {
        return $this->isSome ? $this->value : null;
    }

    /**
     * Get the value, or throw if None.
     *
     * @return T
     * @throws \RuntimeException
     */
    public function unwrap(): mixed
    {
        if (!$this->isSome) {
            throw new \RuntimeException('Called unwrap() on None');
        }
        return $this->value;
    }

    /**
     * Get the value, or return the default.
     *
     * @template D
     * @param D $default
     * @return T|D
     */
    public function unwrapOr(mixed $default): mixed
    {
        return $this->isSome ? $this->value : $default;
    }

    /**
     * Get the value, or call the fallback function.
     *
     * @template D
     * @param callable(): D $fn
     * @return T|D
     */
    public function unwrapOrElse(callable $fn): mixed
    {
        return $this->isSome ? $this->value : $fn();
    }

    /**
     * Map the value if present.
     *
     * @template U
     * @param callable(T): U $fn
     * @return Option<U>
     */
    public function map(callable $fn): self
    {
        if ($this->isSome) {
            return self::some($fn($this->value));
        }
        return $this;
    }

    /**
     * Map the value if present, or return the default.
     *
     * @template U
     * @param U $default
     * @param callable(T): U $fn
     * @return U
     */
    public function mapOr(mixed $default, callable $fn): mixed
    {
        return $this->isSome ? $fn($this->value) : $default;
    }

    /**
     * Chain another operation that returns an Option.
     *
     * @template U
     * @param callable(T): Option<U> $fn
     * @return Option<U>
     */
    public function andThen(callable $fn): self
    {
        if ($this->isSome) {
            return $fn($this->value);
        }
        return $this;
    }

    /**
     * Return this option if Some, otherwise return the other.
     *
     * @param Option<T> $option
     * @return Option<T>
     */
    public function or(self $option): self
    {
        return $this->isSome ? $this : $option;
    }

    /**
     * Convert to a Result with the given error for None.
     *
     * @param ProvenError $error
     * @return Result<T>
     */
    public function okOr(ProvenError $error): Result
    {
        return $this->isSome ? Result::ok($this->value) : Result::err($error);
    }

    /**
     * Filter the option based on a predicate.
     *
     * @param callable(T): bool $predicate
     * @return Option<T>
     */
    public function filter(callable $predicate): self
    {
        if ($this->isSome && $predicate($this->value)) {
            return $this;
        }
        return self::none();
    }

    /**
     * Check if Some and the value matches a predicate.
     *
     * @param callable(T): bool $predicate
     */
    public function isSomeAnd(callable $predicate): bool
    {
        return $this->isSome && $predicate($this->value);
    }
}
