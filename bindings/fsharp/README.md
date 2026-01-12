# Proven for F#

Safe, validated operations library for F# applications.

## Installation

```bash
dotnet add package Proven
```

Or add to your `.fsproj`:

```xml
<PackageReference Include="Proven" Version="0.1.0" />
```

## Usage

```fsharp
open Proven

// Safe math with overflow checking
match SafeMath.add 1000000000000L 2000000000000L with
| Some result -> printfn "Sum: %d" result
| None -> printfn "Overflow!"

// XSS prevention
let safeHtml = SafeString.escapeHtml "<script>alert('xss')</script>"
// "&lt;script&gt;alert('xss')&lt;/script&gt;"

// Path safety
if SafePath.hasTraversal "../../../etc/passwd" then
    failwith "Dangerous path!"

// Email validation
match SafeEmail.parse "user@example.com" with
| SafeEmail.Ok(localPart, domain) -> printfn "Local: %s, Domain: %s" localPart domain
| SafeEmail.Error msg -> printfn "Invalid: %s" msg

// IP classification
match SafeNetwork.parseIPv4 "192.168.1.1" with
| Some ip ->
    printfn "Private: %b" (SafeNetwork.isPrivate ip)
    printfn "Classification: %A" (SafeNetwork.classify ip)
| None -> printfn "Invalid IP"

// Secure tokens
let token = SafeCrypto.generateTokenDefault()
printfn "CSRF token: %s" token
```

## Modules

### SafeMath

Overflow-checked arithmetic using `Option`:

```fsharp
open Proven

// All operations return Option
SafeMath.add a b
SafeMath.sub a b
SafeMath.mul a b
SafeMath.div a b

// Pattern matching
match SafeMath.mul bigNumber anotherBig with
| Some result -> printfn "%d" result
| None -> printfn "Overflow!"

// Default values
SafeMath.div x y |> Option.defaultValue 0L

// Piping
Some 100L
|> Option.bind (fun x -> SafeMath.mul x 200L)
|> Option.bind (fun x -> SafeMath.add x 50L)

// List operations
SafeMath.safeSum [1L; 2L; 3L; 4L; 5L] // Some 15L
SafeMath.safeProduct [2L; 3L; 4L]      // Some 24L
```

### SafeString

XSS prevention and string sanitization:

```fsharp
open Proven

SafeString.escapeHtml "<script>"     // "&lt;script&gt;"
SafeString.escapeSql "O'Brien"        // "O''Brien"
SafeString.escapeJs userInput
SafeString.urlEncode "hello world"    // "hello%20world"
SafeString.sanitizeDefault input      // Only alphanumeric + _-
SafeString.slugify "Hello World!"     // "hello-world"
```

### SafePath

Directory traversal protection:

```fsharp
open Proven

// Check for traversal
if SafePath.hasTraversal userPath then
    failwith "Path traversal detected"

// Sanitize filename
SafePath.sanitizeFilename "../../../etc/passwd" // "etc_passwd"

// Safe path joining
match SafePath.join "/base" ["subdir"; "file.txt"] with
| SafePath.Ok path -> printfn "%s" path
| SafePath.Error msg -> printfn "Error: %s" msg

// Resolve within base directory
match SafePath.resolveWithin "/var/www" userPath with
| SafePath.Ok path -> // Safe to use
| SafePath.Error _ -> // Path escape attempt
```

### SafeEmail

Email validation with discriminated unions:

```fsharp
open Proven

// Simple validation
if SafeEmail.isValid email then ...

// Parse with pattern matching
match SafeEmail.parse email with
| SafeEmail.Ok(localPart, domain) ->
    printfn "Local: %s, Domain: %s" localPart domain
| SafeEmail.Error msg ->
    printfn "Error: %s" msg

// Check for disposable emails
SafeEmail.isDisposable "user@mailinator.com" // true

// Normalize
SafeEmail.normalize "User@EXAMPLE.COM" // Some "User@example.com"
```

### SafeNetwork

IP address validation and classification:

```fsharp
open Proven

// Parse IPv4
match SafeNetwork.parseIPv4 "192.168.1.1" with
| Some ip ->
    printfn "Loopback: %b" (SafeNetwork.isLoopback ip)
    printfn "Private: %b" (SafeNetwork.isPrivate ip)
    match SafeNetwork.classify ip with
    | SafeNetwork.Private -> printfn "Private network"
    | SafeNetwork.Loopback -> printfn "Localhost"
    | SafeNetwork.Public -> printfn "Public IP"
    | _ -> ()
| None -> ()

// Validate port
SafeNetwork.isValidPort 8080 // true

// SSRF protection
if SafeNetwork.isPrivateUrl url then
    failwith "Cannot access private URLs"
```

### SafeCrypto

Cryptographic operations:

```fsharp
open Proven

// Secure random generation
SafeCrypto.randomBytes 32
SafeCrypto.randomHex 16
SafeCrypto.generateTokenDefault()

// Hashing
SafeCrypto.sha256 "data"
SafeCrypto.sha512 "data"

// HMAC
SafeCrypto.hmacSha256 key message
SafeCrypto.verifyHmacSha256 key message expectedMac

// Constant-time comparison (timing attack prevention)
SafeCrypto.constantTimeEquals actual expected

// Key derivation
SafeCrypto.pbkdf2Default password salt
```

## License

PMPL-1.0
