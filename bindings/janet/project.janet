# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
#
# project.janet - Package configuration for proven-janet.
# Janet bindings for libproven, a formally verified safety library.
# All computation is performed in Idris 2 via the Zig FFI layer.

(declare-project
  :name "proven"
  :description "Janet FFI bindings for libproven - formally verified safety library"
  :author "Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>"
  :license "PMPL-1.0-or-later"
  :url "https://github.com/hyperpolymath/proven"
  :repo "git+https://github.com/hyperpolymath/proven.git"
  :version "0.9.0"
  :dependencies [])

(declare-source
  :source ["proven"])
