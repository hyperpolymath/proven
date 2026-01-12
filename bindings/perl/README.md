# Proven Perl

Code that cannot crash - safe arithmetic, string escaping, path validation, and more.

## Installation

```bash
perl Makefile.PL
make
make test
make install
```

Or via CPAN (when published):

```bash
cpan Proven
```

## Usage

```perl
use Proven::SafeMath qw(div add_checked);
use Proven::SafeString qw(escape_html escape_sql);
use Proven::SafePath qw(has_traversal safe_join);
use Proven::SafeEmail qw(is_valid get_domain);
use Proven::SafeNetwork qw(is_private is_loopback);
use Proven::SafeCrypto qw(constant_time_compare random_bytes);

# Safe division - returns undef instead of dying
my $result = div(10, 0);  # undef, not an error

# Overflow-checked arithmetic
my $sum = add_checked(9223372036854775807, 1);  # undef (overflow)

# XSS prevention
my $safe = escape_html('<script>alert(1)</script>');
# &lt;script&gt;alert(1)&lt;/script&gt;

# SQL injection prevention (prefer parameterized queries!)
my $escaped = escape_sql("O'Brien");  # O''Brien

# Path traversal prevention
if (has_traversal($user_input)) {
    die 'Nice try';
}
my $safe_path = safe_join('/uploads', $filename);

# Email validation
if (is_valid($email)) {
    my $domain = get_domain($email);
}

# IP classification
if (is_private($ip)) {
    # Internal network
}

# Constant-time comparison (timing attack prevention)
if (constant_time_compare($token, $expected)) {
    # Valid
}
```

## Modules

- **Proven::SafeMath**: Overflow/underflow-safe arithmetic
- **Proven::SafeString**: HTML, SQL, JS, URL escaping and UTF-8 validation
- **Proven::SafePath**: Directory traversal prevention and filename sanitization
- **Proven::SafeEmail**: Email validation, parsing, and normalization
- **Proven::SafeNetwork**: IPv4/IPv6 validation and classification
- **Proven::SafeCrypto**: Constant-time comparison and secure random

## Requirements

- Perl 5.14+
- Core modules only (Encode, File::Spec, Cwd, Exporter)

## License

PMPL-1.0
