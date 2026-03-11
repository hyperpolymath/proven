# API Reference

Complete API reference for all proven modules.

## Core Types

### Result<E, T>

The fundamental error-handling type used throughout proven.

```idris
data Result : Type -> Type -> Type where
  Ok  : (value : t) -> Result e t
  Err : (error : e) -> Result e t
```

**Functions:**
- `isOk : Result e t -> Bool`
- `isErr : Result e t -> Bool`
- `unwrap : Result e t -> Maybe t`
- `unwrapOr : t -> Result e t -> t`
- `map : (a -> b) -> Result e a -> Result e b`
- `flatMap : (a -> Result e b) -> Result e a -> Result e b`

### NonEmpty<T>

A list guaranteed to have at least one element.

```idris
data NonEmpty : Type -> Type where
  MkNonEmpty : (head : t) -> (tail : List t) -> NonEmpty t
```

### Bounded

A value constrained to a range.

```idris
data Bounded : (min : Integer) -> (max : Integer) -> Type where
  MkBounded : (value : Integer) ->
              {auto prf : (value >= min, value <= max)} ->
              Bounded min max
```

---

## SafeMath

Arithmetic operations that cannot overflow or crash.

### Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `safeAdd` | `Integer -> Integer -> Result OverflowError Integer` | Addition with overflow detection |
| `safeSub` | `Integer -> Integer -> Result OverflowError Integer` | Subtraction with underflow detection |
| `safeMul` | `Integer -> Integer -> Result OverflowError Integer` | Multiplication with overflow detection |
| `safeDiv` | `Integer -> Integer -> Result DivisionError Integer` | Division (returns error if divisor is 0) |
| `safeMod` | `Integer -> Integer -> Result DivisionError Integer` | Modulo (returns error if divisor is 0) |
| `safeAbs` | `Integer -> Integer` | Absolute value |
| `safeNegate` | `Integer -> Integer` | Negation |
| `detectOverflow` | `Integer -> Integer -> Bool` | Check if addition would overflow |

### Example

```python
from proven import SafeMath

result = SafeMath.div(10, 0)  # Returns Err(DivisionByZero)
result = SafeMath.div(10, 2)  # Returns Ok(5)

# With unwrap_or
value = SafeMath.div(10, user_input).unwrap_or(0)
```

---

## SafeString

UTF-8 safe string operations.

### Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `isValidUtf8` | `String -> Bool` | Check if string is valid UTF-8 |
| `escapeHtml` | `String -> String` | Escape HTML special characters |
| `escapeJs` | `String -> String` | Escape for JavaScript strings |
| `escapeSql` | `String -> String` | Escape SQL strings |
| `urlEncode` | `String -> String` | URL-encode a string |
| `urlDecode` | `String -> Result DecodeError String` | URL-decode a string |
| `trim` | `String -> String` | Remove leading/trailing whitespace |
| `safeLength` | `String -> Nat` | Get character count (not byte count) |

### Example

```javascript
import { SafeString } from '@proven/javascript';

const safe = SafeString.escapeHtml('<script>alert("xss")</script>');
// Returns: &lt;script&gt;alert(&quot;xss&quot;)&lt;/script&gt;
```

---

## SafeJson

Exception-free JSON parsing with type-safe access.

### Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `parseJson` | `String -> Result ParseError JsonValue` | Parse JSON string |
| `getString` | `JsonValue -> String -> Result AccessError String` | Get string field |
| `getInt` | `JsonValue -> String -> Result AccessError Integer` | Get integer field |
| `getBool` | `JsonValue -> String -> Result AccessError Bool` | Get boolean field |
| `getArray` | `JsonValue -> String -> Result AccessError (List JsonValue)` | Get array field |
| `getObject` | `JsonValue -> String -> Result AccessError JsonValue` | Get object field |
| `getPath` | `JsonValue -> List String -> Result AccessError JsonValue` | Get nested field |
| `isValidJson` | `String -> Bool` | Check if string is valid JSON |

### Example

```rust
use proven::SafeJson;

let json = SafeJson::parse(r#"{"user": {"name": "Alice"}}"#)?;
let name = SafeJson::get_path(&json, &["user", "name"])?;
```

---

## SafeUrl

RFC 3986 compliant URL parsing.

### Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `parseUrl` | `String -> Result ParseError Url` | Parse URL string |
| `getScheme` | `Url -> String` | Get URL scheme |
| `getHost` | `Url -> String` | Get hostname |
| `getPort` | `Url -> Result NoPort Integer` | Get port number |
| `getPath` | `Url -> String` | Get path |
| `getQuery` | `Url -> Result NoQuery String` | Get query string |
| `getQueryParam` | `Url -> String -> Result NotFound String` | Get query parameter |
| `buildUrl` | `String -> String -> String -> List (String, String) -> String` | Build URL from parts |
| `isValidUrl` | `String -> Bool` | Validate URL format |

### Security

Blocked schemes: `javascript:`, `data:`, `vbscript:`, `file:`

---

## SafeEmail

RFC 5321/5322 email validation.

### Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `parseEmail` | `String -> Result ParseError Email` | Parse email address |
| `getLocalPart` | `Email -> String` | Get local part (before @) |
| `getDomain` | `Email -> String` | Get domain part (after @) |
| `normalizeEmail` | `Email -> String` | Normalize email (lowercase domain) |
| `isValidEmail` | `String -> Bool` | Quick validation check |

---

## SafePath

Filesystem path operations with traversal prevention.

### Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `parsePath` | `String -> Result PathError SafePath` | Parse and validate path |
| `joinPath` | `String -> String -> Result TraversalError String` | Join paths safely |
| `getDirectory` | `SafePath -> String` | Get directory component |
| `getFilename` | `SafePath -> String` | Get filename |
| `getExtension` | `SafePath -> String` | Get file extension |
| `containsTraversal` | `String -> Bool` | Check for `..` traversal |
| `normalizePath` | `String -> String` | Normalize path separators |
| `matchGlob` | `String -> String -> Bool` | Match glob pattern |

### Security

- Blocks `..` path traversal
- Blocks null bytes in paths
- Validates path length limits

---

## SafeSQL

SQL injection prevention with parameterized queries.

### Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `parameterizedQuery` | `SQLDialect -> String -> List SQLValue -> ParameterizedQuery` | Create parameterized query |
| `validateIdentifier` | `String -> Result ValidationError SafeIdentifier` | Validate table/column name |
| `escapeValue` | `SQLDialect -> SQLValue -> String` | Escape value for dialect |
| `select` | `List String -> QueryBuilder` | Start SELECT query |
| `safeInsert` | `String -> List (String, SQLValue) -> ParameterizedQuery` | Create INSERT |
| `safeUpdate` | `String -> List (String, SQLValue) -> String -> List SQLValue -> ParameterizedQuery` | Create UPDATE |
| `safeDelete` | `String -> String -> List SQLValue -> ParameterizedQuery` | Create DELETE |
| `detectInjection` | `String -> Bool` | Detect injection patterns |

### Supported Dialects

- PostgreSQL
- MySQL
- SQLite
- MSSQL
- Oracle

---

## SafeRegex

ReDoS-safe regular expressions.

### Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `regex` | `String -> Result ParseError Regex` | Compile regex |
| `safeRegex` | `SafetyLevel -> String -> Result ParseError SafeRegex` | Compile with safety level |
| `test` | `Regex -> String -> Bool` | Test if pattern matches |
| `match` | `Regex -> String -> Maybe Match` | Find first match |
| `safeMatch` | `SafeRegex -> String -> Result StepLimitExceeded Match` | Match with step limit |
| `replaceAll` | `Regex -> String -> String -> String` | Replace all matches |
| `split` | `Regex -> String -> List String` | Split by pattern |
| `detectReDoS` | `String -> Bool` | Detect ReDoS-vulnerable pattern |
| `analyzeComplexity` | `String -> ComplexityLevel` | Analyze pattern complexity |

### Safety Levels

- `Strict` - Maximum safety, lowest step limit
- `Normal` - Balanced safety and performance
- `Relaxed` - Higher limits for trusted patterns

---

## SafeHTML

XSS prevention with type-safe HTML construction.

### Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `escapeContent` | `String -> String` | Escape HTML content |
| `escapeAttribute` | `String -> String` | Escape for attributes |
| `sanitizeUrl` | `String -> Result SecurityError String` | Sanitize URL for href |
| `elem` | `String -> HtmlBuilder` | Start building element |
| `withAttr` | `HtmlBuilder -> String -> String -> HtmlBuilder` | Add attribute |
| `withText` | `HtmlBuilder -> String -> HtmlBuilder` | Add text content |
| `withChild` | `HtmlBuilder -> TrustedHtml -> HtmlBuilder` | Add child element |
| `build` | `HtmlBuilder -> Result ValidationError TrustedHtml` | Build trusted HTML |
| `sanitize` | `SanitizeConfig -> String -> Result SecurityError String` | Sanitize HTML |
| `isBlacklistedTag` | `String -> Bool` | Check if tag is blacklisted |

---

## SafeJWT

JWT token handling with security validations.

### Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `decodeJwt` | `String -> Result DecodeError JWT` | Decode JWT (no verification) |
| `validateJwt` | `JWT -> ValidationOptions -> SigningKey -> Result ValidationError JWT` | Validate JWT |
| `getClaim` | `JWT -> String -> Result NotFound String` | Get claim value |
| `getAlgorithm` | `JWT -> Algorithm` | Get signing algorithm |
| `isExpired` | `JWT -> Integer -> Bool` | Check if token expired |
| `base64UrlEncode` | `String -> String` | Encode as base64url |
| `base64UrlDecode` | `String -> Result DecodeError String` | Decode base64url |
| `isSecureAlgorithm` | `Algorithm -> Bool` | Check if algorithm is secure |

### Supported Algorithms

HS256, HS384, HS512, RS256, RS384, RS512, ES256, ES384, ES512, PS256, PS384, PS512, EdDSA

---

## Additional Modules

### SafeBase64
Encoding/decoding with variants: Standard, URLSafe, URLSafeNoPad, MIME

### SafeXML
XML parsing with XXE prevention and entity expansion limits

### SafeYAML
YAML parsing with alias bomb prevention and dangerous tag blocking

### SafeTOML
TOML parsing with resource limits

### SafeUUID
RFC 4122 UUID parsing, generation, and validation

### SafeCurrency
ISO 4217 currency handling with safe money arithmetic

### SafePhone
E.164 phone number parsing and validation

### SafeHex
Hex encoding/decoding with bounds checking

### SafeEnv
Environment variable access with sensitivity detection

### SafeArgs
CLI argument parsing with validation

### SafeFile
Bounded file operations with traversal prevention

### SafeHeader
HTTP header validation with CRLF injection prevention

### SafeCookie
Cookie parsing with security attribute handling

### SafeContentType
MIME type validation with sniffing prevention

### SafeCrypto
Hash functions and secure random generation

### SafePassword
Password policy validation and secure hashing

### SafeDateTime
ISO 8601 date/time parsing with timezone handling

### SafeNetwork
IPv4/IPv6 parsing, CIDR notation, port validation

### SafeCommand
Shell command building with injection prevention

---

## Error Types

All modules return typed errors via `Result`:

```idris
data SafeMathError = OverflowError | UnderflowError | DivisionByZero
data ParseError = InvalidFormat String | UnexpectedChar Char Nat
data ValidationError = TooLong | TooShort | InvalidCharacter | ...
data SecurityError = InjectionDetected | TraversalAttempt | ...
```

See individual module documentation for specific error types.
