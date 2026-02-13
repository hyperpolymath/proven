# Proven TUI

Terminal user interface for exploring and demonstrating [Proven](https://github.com/hyperpolymath/proven) safety primitives.

```
 ____
|  _ \ _ __ _____   _____ _ __
| |_) | '__/ _ \ \ / / _ \ '_ \
|  __/| | | (_) \ V /  __/ | | |
|_|   |_|  \___/ \_/ \___|_| |_|

   Verified Safety Primitives
```

## We Eat Our Own Dog Food

This TUI is built using the [proven crate](https://crates.io/crates/proven), demonstrating our commitment to the library. The tick counter uses `SafeMath::checked_add`, the calculator demonstrates overflow protection, and throughout the codebase we practice what we preach.

## Features

- **Interactive Calculator** - Try SafeMath operations and see overflow protection in action
- **Validation Examples** - Explore port, email, and range validators
- **Bounded Types** - Learn about correct-by-construction programming
- **Beautiful TUI** - Built with [ratatui](https://ratatui.rs)

## Installation

```bash
cargo install proven-tui
```

Or build from source:

```bash
git clone https://github.com/hyperpolymath/proven-tui
cd proven-tui
cargo build --release
```

## Usage

```bash
# Run the TUI
proven-tui

# With debug logging
proven-tui --debug

# Custom tick rate (ms)
proven-tui --tick-rate 100
```

## Keyboard Shortcuts

| Key | Action |
|-----|--------|
| `Tab` | Next tab |
| `Shift+Tab` | Previous tab |
| `1-5` | Jump to tab |
| `i` | Enter edit mode (SafeMath tab) |
| `Enter` | Evaluate expression |
| `Esc` | Cancel editing |
| `j/k` or `↑/↓` | Navigate menu |
| `?` | Show help |
| `q` | Quit |

## Try These Expressions

In the SafeMath tab, press `i` and try:

```
42 + 7                          # Basic addition
9223372036854775807 + 1         # Watch overflow get caught!
overflow demo                   # Demonstrate overflow protection
divzero demo                    # Division by zero protection
max i64                         # Show maximum i64 value
```

## Dependencies

- **proven** - The safety primitives library
- **ratatui** - Terminal UI framework
- **crossterm** - Cross-platform terminal manipulation
- **tokio** - Async runtime
- **clap** - CLI argument parsing

## License

PMPL-1.0

## See Also

- [Proven](https://github.com/hyperpolymath/proven) - The core library
- [Proven Malbolge Toolchain](https://github.com/hyperpolymath/proven-malbolge-toolchain) - If we can make Malbolge safe...
