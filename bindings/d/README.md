# Proven for D

Safe, validated operations library for D applications.

## Installation

Add to your `dub.sdl`:

```sdl
dependency "proven" version="~>0.1.0"
```

Or `dub.json`:

```json
{
    "dependencies": {
        "proven": "~>0.1.0"
    }
}
```

## Usage

```d
import proven;

void main()
{
    // Safe math with overflow checking
    auto result = safeAdd(1000000000000L, 2000000000000L);
    if (!result.isNull)
        writefln("Sum: %d", result.get);
    else
        writeln("Overflow!");

    // XSS prevention
    auto safeHtml = escapeHtml("<script>alert('xss')</script>");
    // "&lt;script&gt;alert('xss')&lt;/script&gt;"

    // Path safety
    if (hasTraversal("../../../etc/passwd"))
        throw new Exception("Dangerous path!");

    // Email validation
    auto emailResult = parseEmail("user@example.com");
    if (emailResult.ok)
        writefln("Local: %s, Domain: %s", emailResult.localPart, emailResult.domain);
    else
        writefln("Invalid: %s", emailResult.error);

    // IP classification
    auto ip = parseIPv4("192.168.1.1");
    if (!ip.isNull)
    {
        writefln("Private: %s", isPrivateIP(ip.get));
        writefln("Classification: %s", classifyIP(ip.get));
    }

    // Secure tokens
    auto token = generateTokenDefault();
    writefln("CSRF token: %s", token);
}
```

## Modules

### safe_math

Overflow-checked arithmetic using `Nullable`:

```d
import proven.safe_math;

// All operations return Nullable
safeAdd(a, b);  // Nullable!long
safeSub(a, b);  // Nullable!long
safeMul(a, b);  // Nullable!long
safeDiv(a, b);  // Nullable!long

// Check for overflow
auto result = safeMul(bigNumber, anotherBig);
if (!result.isNull)
    writeln(result.get);
else
    writeln("Overflow!");

// Array operations
safeSum([1L, 2L, 3L, 4L, 5L]);      // Nullable!long(15)
safeProduct([2L, 3L, 4L]);          // Nullable!long(24)
```

### safe_string

XSS prevention and string sanitization:

```d
import proven.safe_string;

escapeHtml("<script>");          // "&lt;script&gt;"
escapeSql("O'Brien");            // "O''Brien"
escapeJs(userInput);
urlEncode("hello world");        // "hello%20world"
sanitizeDefault(input);          // Only alphanumeric + _-
slugify("Hello World!");         // "hello-world"
```

### safe_path

Directory traversal protection:

```d
import proven.safe_path;

// Check for traversal
if (hasTraversal(userPath))
    throw new Exception("Path traversal detected");

// Sanitize filename
sanitizeFilename("../../../etc/passwd");  // "etc_passwd"

// Safe path joining
auto result = pathJoin("/base", "subdir", "file.txt");
if (result.ok)
    writeln(result.path);
else
    writefln("Error: %s", result.error);

// Resolve within base directory
auto resolved = resolveWithin("/var/www", userPath);
if (resolved.ok)
    // Safe to use resolved.path
else
    // Path escape attempt
```

### safe_email

Email validation with result types:

```d
import proven.safe_email;

// Simple validation
if (isValidEmail(email))
    // Valid format

// Parse with result checking
auto result = parseEmail(email);
if (result.ok)
    writefln("Local: %s, Domain: %s", result.localPart, result.domain);
else
    writefln("Error: %s", result.error);

// Check for disposable emails
isDisposableEmail("user@mailinator.com");  // true

// Normalize
normalizeEmail("User@EXAMPLE.COM");  // "User@example.com"
```

### safe_network

IP address validation and classification:

```d
import proven.safe_network;

// Parse IPv4
auto ip = parseIPv4("192.168.1.1");
if (!ip.isNull)
{
    writefln("Loopback: %s", isLoopback(ip.get));
    writefln("Private: %s", isPrivateIP(ip.get));

    final switch (classifyIP(ip.get))
    {
    case IpClassification.private_:
        writeln("Private network");
        break;
    case IpClassification.loopback:
        writeln("Localhost");
        break;
    case IpClassification.public_:
        writeln("Public IP");
        break;
    case IpClassification.reserved:
        writeln("Reserved");
        break;
    case IpClassification.invalid:
        writeln("Invalid");
        break;
    }
}

// Validate port
isValidPort(8080);  // true

// SSRF protection
if (isPrivateUrl(url))
    throw new Exception("Cannot access private URLs");
```

### safe_crypto

Cryptographic operations:

```d
import proven.safe_crypto;

// Secure random generation
randomBytes(32);
randomHex(16);
generateTokenDefault();

// Hashing
sha256Hash("data");
sha512Hash("data");

// HMAC
hmacSha256(key, message);
verifyHmacSha256(key, message, expectedMac);

// Constant-time comparison (timing attack prevention)
constantTimeEqualsString(actual, expected);

// Password generation
generatePasswordDefault();
```

## License

PMPL-1.0
