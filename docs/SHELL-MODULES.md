# Shell-Specific Modules for Valence-Shell Integration

**Date**: 2026-01-29
**Version**: v1.1.0 (planned)
**Status**: Idris2 modules created, Zig FFI pending

---

## New Modules Added

### 1. SafeShell
**Path**: `src/Proven/SafeShell.idr`

**Purpose**: Safe shell syntax validation and injection prevention

**Features**:
- Quote handling (single, double, escape sequences)
- Variable expansion safety (`$VAR`, `${VAR}`)
- Command substitution validation
- Injection detection (command chaining, backgrounding, etc.)
- Safe argument escaping

**Key Functions**:
```idris
escapeForShell : String -> ShellResult String
validateCommand : String -> ShellResult String
detectDangerousPatterns : String -> List DangerousPattern
buildArgs : List String -> List String
```

**Use in valence-shell**: Validate all user input before execution, prevent command injection

---

### 2. SafePipe
**Path**: `src/Proven/SafePipe.idr`

**Purpose**: Safe FIFO and pipe management with deadlock prevention

**Features**:
- Named pipe (FIFO) creation via `mkfifo(2)`
- Anonymous pipe creation
- Path validation (no traversal)
- Resource lifecycle tracking
- Deadlock detection

**Key Functions**:
```idris
createFifo : String -> Bits32 -> PipeResult FifoId
removeFifo : FifoId -> PipeResult ()
createPipe : PipeResult PipePair
safeClosePipe : TrackedPipe -> PipeResult ()
```

**Use in valence-shell**: Process substitution `<(cmd)` and `>(cmd)` execution

---

### 3. SafeProcess
**Path**: `src/Proven/SafeProcess.idr`

**Purpose**: Safe process lifecycle management with zombie prevention

**Features**:
- Fork and exec with validation
- Process waiting (blocking/non-blocking)
- Exit code handling
- Signal delivery
- Zombie reaping
- Resource tracking

**Key Functions**:
```idris
spawnProcess : String -> List String -> ProcessResult ProcessId
waitForProcess : ProcessId -> ProcessResult ExitStatus
reapZombies : ProcessResult (List (ProcessId, ExitStatus))
sendSignal : ProcessId -> Int -> ProcessResult ()
```

**Use in valence-shell**: External command execution, background jobs, pipelines

---

### 4. SafeSignal
**Path**: `src/Proven/SafeSignal.idr`

**Purpose**: Safe POSIX signal handling with re-entrancy safety

**Features**:
- Signal handler registration
- Signal masking and blocking
- Re-entrancy safety guarantees
- Signal set management
- Validation (cannot catch SIGKILL/SIGSTOP)

**Key Functions**:
```idris
installHandler : Signal -> SignalAction -> SignalResult ()
blockSignals : List Signal -> SignalResult SignalMask
restoreSignals : SignalMask -> SignalResult ()
waitForSignal : SignalMask -> SignalResult Signal
```

**Use in valence-shell**: Ctrl+C handling, SIGCHLD for background jobs, SIGTSTP for job control

---

### 5. SafeTerminal
**Path**: `src/Proven/SafeTerminal.idr`

**Purpose**: Safe terminal control and job control

**Features**:
- TTY detection
- Terminal mode management (raw, cooked, cbreak)
- Job control (foreground/background process groups)
- Terminal size detection
- Session management

**Key Functions**:
```idris
isTTY : Int -> TermResult Bool
setRawMode : TermFd -> TermResult TermAttr
getForegroundPgid : TermFd -> TermResult ProcessGroupId
setForegroundPgid : TermFd -> ProcessGroupId -> TermResult ()
initShellTerminal : TermResult (TermFd, TermAttr, ProcessGroupId)
```

**Use in valence-shell**: Interactive mode detection, job control, Ctrl+Z handling

---

## Integration Plan

### Phase 1: Compile Idris2 Modules
```bash
cd ~/Documents/hyperpolymath-repos/proven
idris2 --build proven.ipkg
```

### Phase 2: Create Zig FFI Bindings
Each module needs Zig FFI implementations:
- `ffi/zig/src/shell.zig` - SafeShell FFI
- `ffi/zig/src/pipe.zig` - SafePipe FFI (mkfifo, pipe)
- `ffi/zig/src/process.zig` - SafeProcess FFI (fork, exec, wait)
- `ffi/zig/src/signal.zig` - SafeSignal FFI (sigaction, sigprocmask)
- `ffi/zig/src/terminal.zig` - SafeTerminal FFI (tcgetattr, tcsetpgrp)

### Phase 3: Call from Valence-Shell (Rust)
```rust
// In valence-shell/src/process_substitution.rs
#[link(name = "proven_shell")]
extern "C" {
    fn idris_create_fifo(path: *const u8, len: usize, perms: u32) -> i32;
    fn idris_remove_fifo(path: *const u8, len: usize) -> i32;
}
```

---

## Proofs Deferred

Each module has proof stubs that need implementation:
- **SafeShell**: Escaped strings contain no unquoted metacharacters
- **SafePipe**: FIFO paths contain no traversal, pipe ends are distinct
- **SafeProcess**: Spawned PIDs are positive, reaped processes not zombies
- **SafeSignal**: SIGKILL/SIGSTOP cannot be caught, mask operations idempotent
- **SafeTerminal**: Valid FDs are non-negative, setpgrp operations safe

These proofs will be implemented as the Idris2 code matures.

---

## Why This Matters for Valence-Shell

**Current Status**: Valence-shell v0.10.0 has shell features in Rust without formal verification.

**After Integration**:
- ✅ Process spawning proven correct (no zombies, clean exit)
- ✅ Signal handling proven safe (no race conditions)
- ✅ FIFO management proven leak-free (no orphaned pipes)
- ✅ Shell syntax proven injection-free (no command injection)
- ✅ Job control proven correct (no terminal conflicts)

**Trust Chain**:
```
Idris2 (proven modules)
  ↓ Compiled with totality checking
Zig FFI (thin C wrapper)
  ↓ Direct syscalls (fork, exec, etc.)
Rust (valence-shell)
  ↓ Calls Zig via extern "C"
User commands execute safely
```

---

## Next Steps

1. ✅ Create Idris2 modules (DONE)
2. ✅ Update proven.ipkg (DONE)
3. ⏳ Build proven package (test compilation)
4. ⏳ Create Zig FFI implementations
5. ⏳ Test Idris → Zig → Rust integration
6. ⏳ Integrate with valence-shell milestones:
   - M7: Use SafePipe for process substitution
   - M8: Use SafeShell for arithmetic expansion
   - M10: Use SafeProcess, SafeSignal, SafeTerminal for job control

---

**Co-Authored-By**: Claude Sonnet 4.5 <noreply@anthropic.com>
