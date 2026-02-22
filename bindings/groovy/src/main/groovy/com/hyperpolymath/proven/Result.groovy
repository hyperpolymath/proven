// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// Result type for Groovy-idiomatic error handling.
// Wraps Optional values from JNA calls with Groovy sugar.
package com.hyperpolymath.proven

import groovy.transform.CompileStatic
import groovy.transform.Immutable

/**
 * Result type for exception-free error handling.
 * Groovy-idiomatic with closure support.
 */
@CompileStatic
sealed interface Result<T, E> permits Ok, Err {

    boolean isOk()
    boolean isErr()
    T getValue()
    E getError()

    default <U> Result<U, E> map(Closure<U> mapper) {
        isOk() ? ok(mapper.call(value)) : (Result<U, E>) this
    }

    default <U> Result<U, E> flatMap(Closure<Result<U, E>> mapper) {
        isOk() ? mapper.call(value) : (Result<U, E>) this
    }

    default T orElse(Closure<T> defaultSupplier) {
        isOk() ? value : defaultSupplier.call()
    }

    default T orElse(T defaultValue) {
        isOk() ? value : defaultValue
    }

    default Result<T, E> onSuccess(Closure<?> action) {
        if (isOk()) action.call(value)
        this
    }

    default Result<T, E> onError(Closure<?> action) {
        if (isErr()) action.call(error)
        this
    }

    default <R> R match(Closure<R> onOk, Closure<R> onErr) {
        isOk() ? onOk.call(value) : onErr.call(error)
    }

    static <T, E> Result<T, E> ok(T value) {
        new Ok<T, E>(value)
    }

    static <T, E> Result<T, E> err(E error) {
        new Err<T, E>(error)
    }
}

@Immutable
@CompileStatic
final class Ok<T, E> implements Result<T, E> {
    T value

    @Override boolean isOk() { true }
    @Override boolean isErr() { false }
    @Override T getValue() { value }
    @Override E getError() { null }

    @Override
    String toString() { "Ok(${value})" }
}

@Immutable
@CompileStatic
final class Err<T, E> implements Result<T, E> {
    E error

    @Override boolean isOk() { false }
    @Override boolean isErr() { true }
    @Override T getValue() { null }
    @Override E getError() { error }

    @Override
    String toString() { "Err(${error})" }
}
