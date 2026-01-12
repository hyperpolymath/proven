# proven-py

Python bindings for the [proven](https://github.com/hyperpolymath/proven) library - safe, formally verified operations.

## Installation

```bash
pip install proven
```

## Usage

```python
from proven import SafeMath, SafeString, SafeEmail

# Safe math operations
result = SafeMath.add(1, 2)
if result.is_ok():
    print(result.unwrap())  # 3

# Overflow detection
overflow = SafeMath.add(2**63 - 1, 1)
if overflow.is_err():
    print("Overflow detected!")

# Safe string escaping
escaped = SafeString.escape_html("<script>alert('xss')</script>")
print(escaped)  # &lt;script&gt;...

# Email validation
if SafeEmail.is_valid("user@example.com"):
    print("Valid email")
```

## License

AGPL-3.0-or-later
