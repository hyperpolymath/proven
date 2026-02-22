# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
#
# Proven Safety Library - Zsh Plugin
#
# Oh-My-Zsh compatible plugin that provides shell functions wrapping proven-cli.
# ALL computation delegates to the formally verified Idris 2 core via the Zig
# FFI bridge. This file does NOT reimplement any safety-critical logic.
#
# Installation (oh-my-zsh):
#   1. Copy this directory to $ZSH_CUSTOM/plugins/proven/
#   2. Add 'proven' to your plugins array in ~/.zshrc:
#        plugins=(... proven)
#   3. Reload: source ~/.zshrc
#
# Installation (manual):
#   source /path/to/proven.zsh
#
# Prerequisites:
#   - proven-cli on PATH, or set PROVEN_CLI env var
#
# All functions print results to stdout and return 0 on success, 1 on failure.

# ---------------------------------------------------------------------------
# Plugin metadata
# ---------------------------------------------------------------------------

PROVEN_PLUGIN_VERSION="0.1.0"
PROVEN_PLUGIN_DIR="${0:A:h}"

# ---------------------------------------------------------------------------
# CLI binary resolution
# ---------------------------------------------------------------------------

# Path to the proven-cli binary (override with env var)
: "${PROVEN_CLI:=proven-cli}"

# Internal: verify the CLI binary is available.
# Returns 0 if found, 1 otherwise (with error message on stderr).
_proven_check_cli() {
    if ! command -v "$PROVEN_CLI" &>/dev/null; then
        echo "proven: ERROR: proven-cli not found. Install from ffi/zig/ or set PROVEN_CLI." >&2
        return 1
    fi
    return 0
}

# Internal: call proven-cli with arguments and print stdout.
# Returns the CLI exit code.
_proven_call() {
    _proven_check_cli || return 1
    "$PROVEN_CLI" "$@"
}

# ---------------------------------------------------------------------------
# Load function files
# ---------------------------------------------------------------------------

# Source all function files from the functions/ subdirectory
if [[ -d "${PROVEN_PLUGIN_DIR}/functions" ]]; then
    for func_file in "${PROVEN_PLUGIN_DIR}"/functions/proven-*; do
        [[ -f "$func_file" ]] && source "$func_file"
    done
fi

# ---------------------------------------------------------------------------
# Completions
# ---------------------------------------------------------------------------

# Load the completion file if present
if [[ -f "${PROVEN_PLUGIN_DIR}/_proven" ]]; then
    fpath=("${PROVEN_PLUGIN_DIR}" $fpath)
fi

# ---------------------------------------------------------------------------
# Convenience aliases
# ---------------------------------------------------------------------------

alias pv-add='proven-safe-add'
alias pv-sub='proven-safe-sub'
alias pv-mul='proven-safe-mul'
alias pv-div='proven-safe-div'
alias pv-mod='proven-safe-mod'
alias pv-email='proven-validate-email'
alias pv-url='proven-validate-url'
alias pv-ip='proven-validate-ipv4'
alias pv-sha256='proven-hash-sha256'
alias pv-hex-enc='proven-hex-encode'
alias pv-hex-dec='proven-hex-decode'
alias pv-json='proven-validate-json'
alias pv-ver='proven-version-compare'

# ---------------------------------------------------------------------------
# Plugin info
# ---------------------------------------------------------------------------

# Display plugin version and status
proven-info() {
    echo "Proven Zsh Plugin v${PROVEN_PLUGIN_VERSION}"
    echo "Plugin directory: ${PROVEN_PLUGIN_DIR}"
    echo "CLI binary: ${PROVEN_CLI}"
    if _proven_check_cli 2>/dev/null; then
        echo "CLI status: available ($(command -v "$PROVEN_CLI"))"
        echo "Library version: $(_proven_call version info 2>/dev/null || echo 'unknown')"
    else
        echo "CLI status: NOT FOUND"
    fi
}
