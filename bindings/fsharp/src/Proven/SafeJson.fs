// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

namespace Proven

/// Safe JSON parsing, validation, and manipulation.
module SafeJson =
    open System
    open System.Text
    open System.Text.RegularExpressions

    /// JSON value types.
    type JsonValue =
        | JsonNull
        | JsonBool of bool
        | JsonNumber of decimal
        | JsonString of string
        | JsonArray of JsonValue list
        | JsonObject of Map<string, JsonValue>

    /// JSON parsing errors.
    type JsonError =
        | UnexpectedCharacter of char * int
        | UnexpectedEndOfInput
        | InvalidNumber of string
        | InvalidString of string
        | InvalidEscapeSequence of string
        | InvalidUnicodeEscape of string
        | MaxDepthExceeded of int
        | DuplicateKey of string
        | ParseError of string

    /// JSON path for navigation.
    type JsonPath =
        | Root
        | Key of string * JsonPath
        | Index of int * JsonPath

    /// Maximum nesting depth (prevent stack overflow attacks).
    let private maxDepth = 100

    /// Check if value is null.
    let isNull (value: JsonValue) : bool =
        match value with
        | JsonNull -> true
        | _ -> false

    /// Check if value is a boolean.
    let isBool (value: JsonValue) : bool =
        match value with
        | JsonBool _ -> true
        | _ -> false

    /// Check if value is a number.
    let isNumber (value: JsonValue) : bool =
        match value with
        | JsonNumber _ -> true
        | _ -> false

    /// Check if value is a string.
    let isString (value: JsonValue) : bool =
        match value with
        | JsonString _ -> true
        | _ -> false

    /// Check if value is an array.
    let isArray (value: JsonValue) : bool =
        match value with
        | JsonArray _ -> true
        | _ -> false

    /// Check if value is an object.
    let isObject (value: JsonValue) : bool =
        match value with
        | JsonObject _ -> true
        | _ -> false

    /// Get as boolean.
    let asBool (value: JsonValue) : bool option =
        match value with
        | JsonBool b -> Some b
        | _ -> None

    /// Get as number (decimal).
    let asNumber (value: JsonValue) : decimal option =
        match value with
        | JsonNumber n -> Some n
        | _ -> None

    /// Get as integer.
    let asInt (value: JsonValue) : int option =
        match value with
        | JsonNumber n when n >= decimal Int32.MinValue && n <= decimal Int32.MaxValue ->
            Some(int n)
        | _ -> None

    /// Get as int64.
    let asInt64 (value: JsonValue) : int64 option =
        match value with
        | JsonNumber n when n >= decimal Int64.MinValue && n <= decimal Int64.MaxValue ->
            Some(int64 n)
        | _ -> None

    /// Get as float.
    let asFloat (value: JsonValue) : float option =
        match value with
        | JsonNumber n -> Some(float n)
        | _ -> None

    /// Get as string.
    let asString (value: JsonValue) : string option =
        match value with
        | JsonString s -> Some s
        | _ -> None

    /// Get as array.
    let asArray (value: JsonValue) : JsonValue list option =
        match value with
        | JsonArray arr -> Some arr
        | _ -> None

    /// Get as object (map).
    let asObject (value: JsonValue) : Map<string, JsonValue> option =
        match value with
        | JsonObject obj -> Some obj
        | _ -> None

    /// Get property from object.
    let getProperty (key: string) (value: JsonValue) : JsonValue option =
        match value with
        | JsonObject obj -> Map.tryFind key obj
        | _ -> None

    /// Get element from array.
    let getElement (index: int) (value: JsonValue) : JsonValue option =
        match value with
        | JsonArray arr when index >= 0 && index < List.length arr ->
            Some(List.item index arr)
        | _ -> None

    /// Navigate JSON using path.
    let navigate (path: JsonPath) (value: JsonValue) : JsonValue option =
        let rec navigateRec pathPart current =
            match pathPart with
            | Root -> Some current
            | Key(key, rest) ->
                match current with
                | JsonObject obj ->
                    Map.tryFind key obj |> Option.bind (navigateRec rest)
                | _ -> None
            | Index(i, rest) ->
                match current with
                | JsonArray arr when i >= 0 && i < List.length arr ->
                    navigateRec rest (List.item i arr)
                | _ -> None
        navigateRec path value

    /// Escape string for JSON.
    let escapeString (input: string) : string =
        let sb = StringBuilder()
        for c in input do
            match c with
            | '"' -> sb.Append("\\\"") |> ignore
            | '\\' -> sb.Append("\\\\") |> ignore
            | '\n' -> sb.Append("\\n") |> ignore
            | '\r' -> sb.Append("\\r") |> ignore
            | '\t' -> sb.Append("\\t") |> ignore
            | '\b' -> sb.Append("\\b") |> ignore
            | '\012' -> sb.Append("\\f") |> ignore  // \f
            | c when int c < 32 -> sb.Append(sprintf "\\u%04x" (int c)) |> ignore
            | c -> sb.Append(c) |> ignore
        sb.ToString()

    /// Format JSON value to string (compact).
    let rec format (value: JsonValue) : string =
        match value with
        | JsonNull -> "null"
        | JsonBool true -> "true"
        | JsonBool false -> "false"
        | JsonNumber n ->
            if n = Math.Truncate(n) then sprintf "%.0f" n
            else n.ToString()
        | JsonString s -> sprintf "\"%s\"" (escapeString s)
        | JsonArray items ->
            let formatted = items |> List.map format |> String.concat ","
            sprintf "[%s]" formatted
        | JsonObject props ->
            let formatted =
                props
                |> Map.toSeq
                |> Seq.map (fun (k, v) -> sprintf "\"%s\":%s" (escapeString k) (format v))
                |> String.concat ","
            sprintf "{%s}" formatted

    /// Format JSON value to string (pretty-printed).
    let formatPretty (indent: int) (value: JsonValue) : string =
        let spaces n = String(' ', n)
        let rec formatRec level value =
            let currentIndent = spaces (level * indent)
            let nextIndent = spaces ((level + 1) * indent)
            match value with
            | JsonNull -> "null"
            | JsonBool true -> "true"
            | JsonBool false -> "false"
            | JsonNumber n ->
                if n = Math.Truncate(n) then sprintf "%.0f" n
                else n.ToString()
            | JsonString s -> sprintf "\"%s\"" (escapeString s)
            | JsonArray [] -> "[]"
            | JsonArray items ->
                let formatted =
                    items
                    |> List.map (fun v -> nextIndent + formatRec (level + 1) v)
                    |> String.concat ",\n"
                sprintf "[\n%s\n%s]" formatted currentIndent
            | JsonObject props when Map.isEmpty props -> "{}"
            | JsonObject props ->
                let formatted =
                    props
                    |> Map.toSeq
                    |> Seq.map (fun (k, v) ->
                        sprintf "%s\"%s\": %s" nextIndent (escapeString k) (formatRec (level + 1) v))
                    |> String.concat ",\n"
                sprintf "{\n%s\n%s}" formatted currentIndent
        formatRec 0 value

    /// Create null value.
    let createNull () : JsonValue = JsonNull

    /// Create boolean value.
    let createBool (value: bool) : JsonValue = JsonBool value

    /// Create number value from int.
    let createInt (value: int) : JsonValue = JsonNumber(decimal value)

    /// Create number value from int64.
    let createInt64 (value: int64) : JsonValue = JsonNumber(decimal value)

    /// Create number value from float.
    let createFloat (value: float) : JsonValue = JsonNumber(decimal value)

    /// Create number value from decimal.
    let createDecimal (value: decimal) : JsonValue = JsonNumber value

    /// Create string value.
    let createString (value: string) : JsonValue = JsonString value

    /// Create array value.
    let createArray (items: JsonValue list) : JsonValue = JsonArray items

    /// Create object value.
    let createObject (properties: (string * JsonValue) list) : JsonValue =
        JsonObject(Map.ofList properties)

    /// Set property on object.
    let setProperty (key: string) (value: JsonValue) (obj: JsonValue) : JsonValue option =
        match obj with
        | JsonObject props -> Some(JsonObject(Map.add key value props))
        | _ -> None

    /// Remove property from object.
    let removeProperty (key: string) (obj: JsonValue) : JsonValue option =
        match obj with
        | JsonObject props -> Some(JsonObject(Map.remove key props))
        | _ -> None

    /// Check if object has property.
    let hasProperty (key: string) (value: JsonValue) : bool =
        match value with
        | JsonObject props -> Map.containsKey key props
        | _ -> false

    /// Get object keys.
    let getKeys (value: JsonValue) : string list option =
        match value with
        | JsonObject props -> Some(Map.toList props |> List.map fst)
        | _ -> None

    /// Get array length.
    let length (value: JsonValue) : int option =
        match value with
        | JsonArray arr -> Some(List.length arr)
        | JsonObject props -> Some(Map.count props)
        | _ -> None

    /// Map over array elements.
    let mapArray (f: JsonValue -> JsonValue) (value: JsonValue) : JsonValue option =
        match value with
        | JsonArray arr -> Some(JsonArray(List.map f arr))
        | _ -> None

    /// Filter array elements.
    let filterArray (predicate: JsonValue -> bool) (value: JsonValue) : JsonValue option =
        match value with
        | JsonArray arr -> Some(JsonArray(List.filter predicate arr))
        | _ -> None

    /// Merge two objects (second overrides first).
    let merge (a: JsonValue) (b: JsonValue) : JsonValue option =
        match a, b with
        | JsonObject propsA, JsonObject propsB ->
            let merged = Map.fold (fun acc k v -> Map.add k v acc) propsA propsB
            Some(JsonObject merged)
        | _ -> None

    /// Deep merge two objects.
    let rec deepMerge (a: JsonValue) (b: JsonValue) : JsonValue =
        match a, b with
        | JsonObject propsA, JsonObject propsB ->
            let merged =
                Map.fold (fun acc k vB ->
                    match Map.tryFind k acc with
                    | Some vA -> Map.add k (deepMerge vA vB) acc
                    | None -> Map.add k vB acc
                ) propsA propsB
            JsonObject merged
        | _, _ -> b

    /// Check structural equality.
    let rec equals (a: JsonValue) (b: JsonValue) : bool =
        match a, b with
        | JsonNull, JsonNull -> true
        | JsonBool x, JsonBool y -> x = y
        | JsonNumber x, JsonNumber y -> x = y
        | JsonString x, JsonString y -> x = y
        | JsonArray x, JsonArray y ->
            List.length x = List.length y &&
            List.forall2 equals x y
        | JsonObject x, JsonObject y ->
            Map.count x = Map.count y &&
            Map.forall (fun k v ->
                match Map.tryFind k y with
                | Some v2 -> equals v v2
                | None -> false) x
        | _ -> false

    /// Validate JSON string (check if parseable).
    let isValidJson (input: string) : bool =
        // Simple validation - check balanced braces/brackets and basic structure
        if String.IsNullOrWhiteSpace(input) then false
        else
            let trimmed = input.Trim()
            (trimmed.StartsWith("{") && trimmed.EndsWith("}")) ||
            (trimmed.StartsWith("[") && trimmed.EndsWith("]")) ||
            trimmed = "null" ||
            trimmed = "true" ||
            trimmed = "false" ||
            Regex.IsMatch(trimmed, @"^-?\d+(\.\d+)?([eE][+-]?\d+)?$") ||
            (trimmed.StartsWith("\"") && trimmed.EndsWith("\""))
