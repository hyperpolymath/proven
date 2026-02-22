# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

# meson.build -- Build configuration for Proven Vala bindings.
#
# This builds a shared library (libproven-vala) that provides idiomatic
# GLib/GObject wrapper classes over the libproven C ABI.  The wrappers
# contain NO reimplemented logic; all computation is performed inside
# libproven (Idris 2 + Zig FFI).
#
# Build:
#   meson setup build
#   meson compile -C build
#
# Requirements:
#   - valac >= 0.56
#   - glib-2.0 >= 2.70
#   - gobject-2.0 >= 2.70
#   - libproven (in system library path or via PKG_CONFIG_PATH)

project(
    'proven-vala',
    'vala', 'c',
    version : '0.9.0',
    license : 'PMPL-1.0-or-later',
    meson_version : '>= 0.62.0',
    default_options : [
        'warning_level=2',
        'c_std=c11',
    ],
)

# Dependencies
glib_dep    = dependency('glib-2.0',   version : '>= 2.70')
gobject_dep = dependency('gobject-2.0', version : '>= 2.70')

# Attempt to find libproven via pkg-config; fall back to manual search.
proven_dep = dependency('proven', required : false)
if not proven_dep.found()
    cc = meson.get_compiler('c')
    proven_dep = cc.find_library('proven', required : true)
endif

# Proven C header include path -- the canonical header lives two levels up.
proven_inc = include_directories('../../bindings/c/include')

# Vala sources in dependency order (VAPI first, then wrapper classes).
vala_sources = files(
    'src/SafeMath.vala',
    'src/SafeString.vala',
    'src/SafePath.vala',
    'src/SafeEmail.vala',
    'src/SafeUrl.vala',
    'src/SafeNetwork.vala',
    'src/SafeCrypto.vala',
    'src/SafeJson.vala',
    'src/SafeDateTime.vala',
)

# The VAPI file is passed via --vapidir + --pkg so valac picks it up.
vapi_dir = meson.current_source_dir() / 'src'

# Build shared library
libproven_vala = shared_library(
    'proven-vala',
    vala_sources,
    vala_args : [
        '--vapidir=' + vapi_dir,
        '--pkg=lib_proven',
    ],
    dependencies : [glib_dep, gobject_dep, proven_dep],
    include_directories : proven_inc,
    install : true,
)

# Also build as static library for convenience.
libproven_vala_static = static_library(
    'proven-vala-static',
    vala_sources,
    vala_args : [
        '--vapidir=' + vapi_dir,
        '--pkg=lib_proven',
    ],
    dependencies : [glib_dep, gobject_dep, proven_dep],
    include_directories : proven_inc,
    install : true,
)
