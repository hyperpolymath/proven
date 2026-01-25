#!/usr/bin/env bash
# SPDX-License-Identifier: AGPL-3.0-or-later
# EMERGENCY: Delete ALL reimplementation files from bindings
#
# This is a one-time nuclear cleanup. After this, proven-cleaner.sh will
# prevent these files from ever coming back.

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$REPO_ROOT"

BACKUP_DIR=".UNSAFE-ALL-BINDINGS-DELETED-$(date +%Y%m%d-%H%M%S)"
mkdir -p "$BACKUP_DIR"

echo "🚨 EMERGENCY CLEANUP: Deleting ALL reimplementation files from bindings"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo ""
echo "Backup directory: $BACKUP_DIR"
echo ""

TOTAL_DELETED=0

# Delete patterns for ALL languages
PATTERNS=(
    # Scala
    "bindings/scala/src/main/scala/proven/Safe*.scala"

    # PHP
    "bindings/php/src/Proven/Safe*.php"

    # Clojure
    "bindings/clojure/src/proven/safe_*.clj"

    # V
    "bindings/v/src/safe_*.v"

    # ReScript (keep only FFI bindings)
    "bindings/rescript/src/Proven_Safe*.res"

    # JavaScript/TypeScript
    "bindings/javascript/src/safe*.js"
    "bindings/javascript/src/safe*.ts"
    "bindings/typescript/src/safe*.ts"

    # Python
    "bindings/python/proven/safe_*.py"
    "bindings/python/src/proven/safe_*.py"

    # Ruby
    "bindings/ruby/lib/proven/safe_*.rb"

    # Go
    "bindings/go/safe_*.go"

    # Elixir
    "bindings/elixir/lib/proven/safe_*.ex"

    # Haskell
    "bindings/haskell/src/Proven/Safe*.hs"

    # Java
    "bindings/java/src/main/java/proven/Safe*.java"

    # Kotlin
    "bindings/kotlin/src/main/kotlin/proven/Safe*.kt"

    # Swift
    "bindings/swift/Sources/Proven/Safe*.swift"

    # Dart
    "bindings/dart/lib/src/safe_*.dart"

    # Nim
    "bindings/nim/src/proven/safe_*.nim"

    # Crystal
    "bindings/crystal/src/proven/safe_*.cr"

    # Erlang
    "bindings/erlang/src/safe_*.erl"

    # OCaml
    "bindings/ocaml/lib/safe_*.ml"

    # F#
    "bindings/fsharp/src/Safe*.fs"

    # C++
    "bindings/cpp/src/safe_*.cpp"
    "bindings/cpp/include/proven/safe_*.hpp"

    # C
    "bindings/c/src/safe_*.c"
    "bindings/c/include/proven/safe_*.h"

    # Zig (keep only FFI bridge in ffi/zig/)
    "bindings/zig/src/safe_*.zig"

    # Lua
    "bindings/lua/src/safe_*.lua"

    # Perl
    "bindings/perl/lib/Proven/Safe*.pm"

    # R
    "bindings/r/R/safe_*.R"

    # Julia
    "bindings/julia/src/Safe*.jl"

    # Common Lisp
    "bindings/common-lisp/src/safe-*.lisp"

    # Guile Scheme
    "bindings/guile/proven/safe-*.scm"

    # Racket
    "bindings/racket/safe-*.rkt"

    # D
    "bindings/d/source/proven/safe_*.d"

    # Fortran
    "bindings/fortran/src/safe_*.f90"

    # Ada
    "bindings/ada/src/proven-safe_*.ad[sb]"

    # PureScript
    "bindings/purescript/src/Proven/Safe*.purs"

    # Elm
    "bindings/elm/src/Proven/Safe*.elm"

    # Grain
    "bindings/grain/src/safe_*.gr"

    # Gleam
    "bindings/gleam/src/proven/safe_*.gleam"

    # Odin
    "bindings/odin/src/safe_*.odin"
)

for pattern in "${PATTERNS[@]}"; do
    # Check if any files match this pattern
    if compgen -G "$pattern" > /dev/null 2>&1; then
        for file in $pattern; do
            if [ -f "$file" ]; then
                # Backup the file
                backup_path="$BACKUP_DIR/$file"
                mkdir -p "$(dirname "$backup_path")"
                cp "$file" "$backup_path"

                # Delete the file
                rm "$file"
                echo "❌ Deleted: $file"
                ((TOTAL_DELETED++))
            fi
        done
    fi
done

echo ""
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "📊 Cleanup Summary"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "Total files deleted: $TOTAL_DELETED"
echo "Backup location: $BACKUP_DIR"
echo ""

if [ $TOTAL_DELETED -eq 0 ]; then
    echo "✅ No reimplementation files found - bindings are clean!"
    exit 0
fi

echo "⚠️  These files were ALL reimplementations that bypassed Idris2 verification."
echo "⚠️  Language bindings MUST be thin FFI wrappers, not native implementations."
echo ""
echo "Next steps:"
echo "1. Rebuild FFI layer in ffi/zig/"
echo "2. Create minimal FFI wrapper templates for each language"
echo "3. Test that bindings call Idris2 code"
echo "4. Run proven-cleaner.sh to prevent recurrence"
echo ""

exit 1
