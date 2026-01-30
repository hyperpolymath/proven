# ReScript Bindings for Proven

![Idris Inside](https://img.shields.io/badge/Idris-Inside-5E5086?style=flat&logo=idris&logoColor=white)
![License: AGPL-3.0-or-later](https://img.shields.io/badge/License-AGPL--3.0--or--later-blue.svg)

Type-safe ReScript bindings to the [proven](https://github.com/hyperpolymath/proven) formally verified library.

## Features

- **ProvenSafeUrl** - Unbreakable URL parsing and manipulation
- **ProvenSafeString** - String operations that cannot crash
- **ProvenSafeJson** - Safe JSON parsing and manipulation
- **ProvenResult** - Result type for error handling

## Installation

```bash
npm install @proven/rescript-bindings
```

Add to your `rescript.json`:

```json
{
  "bs-dependencies": [
    "@proven/rescript-bindings"
  ]
}
```

## Usage

### SafeUrl

```rescript
open ProvenSafeUrl

// Parse a URL safely
let urlResult = parse("https://example.com/path?q=1")
switch urlResult {
| Ok(url) => Console.log(url.hostname) // "example.com"
| Error(err) => Console.error(err)
}

// Check if URL is valid
if isValid("https://example.com") {
  Console.log("Valid URL!")
}

// Get query parameters
switch getQueryParam("https://example.com?foo=bar", "foo") {
| Ok(Some(value)) => Console.log(value) // "bar"
| Ok(None) => Console.log("Parameter not found")
| Error(err) => Console.error(err)
}
```

### SafeString

```rescript
open ProvenSafeString

// Safe character access
switch charAt("hello", 1) {
| Ok(char) => Console.log(char) // "e"
| Error(err) => Console.error(err) // Index out of bounds
}

// Safe substring
switch substring("hello world", ~start=0, ~end=Some(5)) {
| Ok(sub) => Console.log(sub) // "hello"
| Error(err) => Console.error(err)
}

// String operations
let trimmed = trim("  hello  ") // "hello"
let upper = toUpperCase("hello") // "HELLO"
let parts = split("a,b,c", ",") // ["a", "b", "c"]
```

### SafeJson

```rescript
open ProvenSafeJson

// Parse JSON safely
switch parse("{\"name\": \"Alice\", \"age\": 30}") {
| Ok(json) => {
    switch get(json, "name") {
    | Ok(Some(nameJson)) => Console.log(nameJson)
    | Ok(None) => Console.log("Key not found")
    | Error(err) => Console.error(err)
    }
  }
| Error(err) => Console.error("Parse error: " ++ err)
}

// Check if JSON is valid
if isValid("{\"valid\": true}") {
  Console.log("Valid JSON!")
}

// Navigate nested JSON
let json = parse("{\"user\": {\"name\": \"Bob\"}}")
switch json {
| Ok(j) => {
    switch getPath(j, ["user", "name"]) {
    | Ok(Some(name)) => Console.log(name)
    | _ => ()
    }
  }
| _ => ()
}
```

## Architecture

These bindings provide type-safe ReScript interfaces to proven's formally verified modules:

```
ReScript Application
        ↓
ReScript Bindings (this package)
        ↓
JavaScript Bindings (proven/bindings/javascript)
        ↓
Zig FFI (proven/ffi/zig)
        ↓
Idris2 ABI (proven/src/Proven/*.idr)
        ↓
Formally Verified Implementation
```

## Why Proven?

The proven library uses **Idris2 dependent types** to provide formal proofs of correctness:

- **SafeUrl**: Proves URLs are well-formed and operations maintain validity
- **SafeString**: Proves index bounds and string operations cannot crash
- **SafeJson**: Proves JSON structures are valid and access is safe

These proofs are checked at compile-time in Idris2, ensuring runtime safety in ReScript/JavaScript.

## Integration with rescript-tea

These bindings are designed to integrate seamlessly with [rescript-tea](https://github.com/hyperpolymath/rescript-tea):

```rescript
// Safe URL handling in TEA router
let update = (model, msg) => {
  switch msg {
  | Navigate(urlString) =>
      switch ProvenSafeUrl.parse(urlString) {
      | Ok(url) => ({...model, currentUrl: url}, Tea.Cmd.none)
      | Error(_) => (model, Tea.Cmd.none) // Invalid URL ignored
      }
  | _ => (model, Tea.Cmd.none)
  }
}
```

## Integration with cadre-tea-router

ProvenSafeUrl is the foundation for unbreakable routing in [cadre-tea-router](https://github.com/hyperpolymath/cadre-tea-router):

```rescript
// Route definitions with formally verified URL parsing
let routes = Router.define([
  Router.route("/users/:id", ~parser=ProvenSafeUrl.parse),
  Router.route("/posts/:slug", ~parser=ProvenSafeUrl.parse),
])
```

## License

AGPL-3.0-or-later

## Author

Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk>

## See Also

- [proven](https://github.com/hyperpolymath/proven) - Formally verified library (Idris2)
- [rescript-tea](https://github.com/hyperpolymath/rescript-tea) - The Elm Architecture in ReScript
- [cadre-tea-router](https://github.com/hyperpolymath/cadre-tea-router) - Type-safe routing for TEA
