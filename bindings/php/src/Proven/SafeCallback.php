<?php
// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// SafeCallback - FFI wrapper for proven_callback_* functions.
// All computation delegated to libproven; no logic reimplemented here.

declare(strict_types=1);

namespace Proven;

/**
 * Callback infrastructure via libproven FFI.
 *
 * Every method calls the corresponding libproven FFI function.
 */
final class SafeCallback
{
    /**
     * Get the number of registered callbacks for an event type.
     *
     * @param int $eventType The event type code.
     * @return int The number of registered callbacks.
     */
    public static function count(int $eventType): int
    {
        return (int) FFI::getLib()->proven_callback_count($eventType);
    }

    /**
     * Clear all registered callbacks.
     *
     * @return int The number of callbacks that were cleared.
     */
    public static function clearAll(): int
    {
        return (int) FFI::getLib()->proven_callback_clear_all();
    }
}
