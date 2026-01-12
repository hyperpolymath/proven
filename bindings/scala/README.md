# Proven for Scala

Safe, validated operations library for Scala applications.

## Installation

Add to your `build.sbt`:

```scala
libraryDependencies += "com.hyperpolymath" %% "proven" % "0.1.0"
```

Or clone and publish locally:

```bash
sbt publishLocal
```

## Usage

```scala
import proven._

// Safe math with overflow checking
SafeMath.add(1000000000000L, 2000000000000L) match {
  case Some(result) => println(s"Sum: $result")
  case None => println("Overflow!")
}

// XSS prevention
val userInput = "<script>alert('xss')</script>"
val safeHtml = SafeString.escapeHtml(userInput)
// &lt;script&gt;alert('xss')&lt;/script&gt;

// Path safety
if (SafePath.hasTraversal("../../../etc/passwd")) {
  throw new SecurityException("Dangerous path!")
}

// Email validation
SafeEmail.parse("user@example.com") match {
  case EmailResult.Ok(local, domain) =>
    println(s"Local: $local, Domain: $domain")
  case EmailResult.Error(msg) =>
    println(s"Invalid: $msg")
}

// IP classification
IPv4Address.parse("192.168.1.1").foreach { ip =>
  println(s"Private: ${ip.isPrivate}")
  println(s"Classification: ${ip.classification}")
}

// Secure tokens
val token = SafeCrypto.generateToken()
println(s"CSRF token: $token")
```

## Modules

### SafeMath

Overflow-checked arithmetic using `Option`:

```scala
// All operations return Option[Long]
val sum = SafeMath.add(a, b)
val diff = SafeMath.sub(a, b)
val prod = SafeMath.mul(a, b)
val quot = SafeMath.div(a, b)

// Pattern matching
SafeMath.mul(bigNumber, anotherBig) match {
  case Some(result) => println(result)
  case None => println("Overflow!")
}

// Default values
val value = SafeMath.div(x, y).getOrElse(0L)

// For comprehension
for {
  a <- SafeMath.add(x, y)
  b <- SafeMath.mul(a, z)
} yield b
```

### SafeString

XSS prevention and string sanitization:

```scala
// HTML escaping
val safe = SafeString.escapeHtml("<script>")

// SQL escaping (prefer parameterized queries!)
val safeSql = SafeString.escapeSql("O'Brien")

// JavaScript escaping
val safeJs = SafeString.escapeJs(userInput)

// URL encoding
val encoded = SafeString.urlEncode("hello world")

// String sanitization
val clean = SafeString.sanitize(input, "a-zA-Z0-9")
```

### SafePath

Directory traversal protection:

```scala
// Check for traversal
if (SafePath.hasTraversal(userPath)) {
  throw new SecurityException("Path traversal detected")
}

// Sanitize filename
val safe = SafePath.sanitizeFilename(userFilename)

// Safe path joining
SafePath.join("/base", "subdir", "file.txt") match {
  case PathResult.Ok(path) => println(path)
  case PathResult.Error(msg) => println(s"Error: $msg")
}

// Resolve within base directory
SafePath.resolveWithin("/var/www", userPath) match {
  case PathResult.Ok(path) => // Safe to use
  case PathResult.Error(_) => // Path escape attempt
}
```

### SafeEmail

Email validation with ADTs:

```scala
// Simple validation
if (SafeEmail.isValid(email)) {
  // Valid
}

// Parse with pattern matching
SafeEmail.parse(email) match {
  case EmailResult.Ok(local, domain) =>
    println(s"Local: $local, Domain: $domain")
  case EmailResult.Error(msg) =>
    println(s"Error: $msg")
}

// Check for disposable emails
if (SafeEmail.isDisposable(email)) {
  println("Please use a non-disposable email")
}

// Normalize
val normalized = SafeEmail.normalize("User@EXAMPLE.COM")
// Some("User@example.com")
```

### SafeNetwork

IP address validation and classification:

```scala
// Parse IPv4
IPv4Address.parse("192.168.1.1").foreach { ip =>
  println(s"Loopback: ${ip.isLoopback}")
  println(s"Private: ${ip.isPrivate}")
  println(s"Public: ${ip.isPublic}")

  ip.classification match {
    case IpClassification.Private => println("Private network")
    case IpClassification.Loopback => println("Localhost")
    case IpClassification.Public => println("Public IP")
    case _ => println("Other")
  }
}

// Validate port
if (SafeNetwork.isValidPort(8080)) {
  // OK
}

// SSRF protection
if (SafeNetwork.isPrivateUrl(url)) {
  throw new SecurityException("Cannot access private URLs")
}
```

### SafeCrypto

Cryptographic operations:

```scala
// Secure random generation
val bytes = SafeCrypto.randomBytes(32)
val hex = SafeCrypto.randomHex(16)
val token = SafeCrypto.generateToken()

// Hashing
val hash = SafeCrypto.sha256("data")
val hash512 = SafeCrypto.sha512("data")

// HMAC
val mac = SafeCrypto.hmacSha256(key, message)
if (SafeCrypto.verifyHmacSha256(key, message, expectedMac)) {
  // Valid
}

// Constant-time comparison (timing attack prevention)
if (SafeCrypto.constantTimeEquals(actual, expected)) {
  // Match
}

// Key derivation
val derived = SafeCrypto.pbkdf2(password, salt, iterations = 100000)
```

## License

PMPL-1.0
