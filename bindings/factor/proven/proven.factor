! SPDX-License-Identifier: PMPL-1.0-or-later
! Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
!
! proven - Main vocabulary for the Proven safety library.
! FFI binding to libproven. All computation is performed in verified
! Idris 2 code via the Zig FFI layer. This vocabulary is a thin wrapper;
! it does NOT reimplement any logic.

USING: kernel proven.ffi ;
IN: proven

! Initialize the Proven runtime. Must be called before using any
! other Proven words. Safe to call multiple times.
! ( -- status )
: proven-init ( -- status )
    proven_init ;

! Cleanup the Proven runtime.
! ( -- )
: proven-deinit ( -- )
    proven_deinit ;

! Check if the runtime is initialized.
! ( -- ? )
: proven-initialized? ( -- ? )
    proven_is_initialized ;

! Get the FFI ABI version.
! ( -- n )
: proven-abi-version ( -- n )
    proven_ffi_abi_version ;

! Get the library version as three numbers on the stack.
! ( -- major minor patch )
: proven-version ( -- major minor patch )
    proven_version_major
    proven_version_minor
    proven_version_patch ;

! Get the total module count.
! ( -- n )
: proven-module-count ( -- n )
    proven_module_count ;
