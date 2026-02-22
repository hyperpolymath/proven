<?php
// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
//
// Proven - Entry point for the Proven PHP binding.
// Provides lifecycle management and version info via libproven FFI.

declare(strict_types=1);

namespace Proven;

/**
 * Entry point for the Proven library.
 *
 * Provides lifecycle management (init/deinit) and version information.
 * The native library must be initialized before calling any other Proven functions.
 *
 * Usage:
 *     Proven::init();
 *     try {
 *         $sum = SafeMath::addChecked(1, 2);
 *     } finally {
 *         Proven::deinit();
 *     }
 */
final class Proven
{
    /**
     * Initialize the Proven library. Must be called before any other operations.
     */
    public static function init(): bool
    {
        return FFI::init();
    }

    /**
     * Deinitialize the Proven library.
     */
    public static function deinit(): void
    {
        FFI::deinit();
    }

    /**
     * Check if the library has been initialized.
     */
    public static function isInitialized(): bool
    {
        return FFI::isInitialized();
    }

    /**
     * Get the FFI ABI version number.
     */
    public static function abiVersion(): int
    {
        return (int) FFI::getLib()->proven_ffi_abi_version();
    }

    /**
     * Get the library major version.
     */
    public static function versionMajor(): int
    {
        return (int) FFI::getLib()->proven_version_major();
    }

    /**
     * Get the library minor version.
     */
    public static function versionMinor(): int
    {
        return (int) FFI::getLib()->proven_version_minor();
    }

    /**
     * Get the library patch version.
     */
    public static function versionPatch(): int
    {
        return (int) FFI::getLib()->proven_version_patch();
    }

    /**
     * Get the library version as a string (e.g. "0.9.0").
     */
    public static function version(): string
    {
        return self::versionMajor() . '.' . self::versionMinor() . '.' . self::versionPatch();
    }

    /**
     * Get the number of modules registered in the library.
     */
    public static function moduleCount(): int
    {
        return (int) FFI::getLib()->proven_module_count();
    }
}
