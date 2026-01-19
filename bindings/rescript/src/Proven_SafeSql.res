// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

open Proven_Bitwise

/**
 * SafeSql - SQL query sanitization and parameterization that cannot crash.
 *
 * Provides utilities for constructing safe SQL queries with proper escaping
 * and parameterized query building. Always prefer parameterized queries over
 * string interpolation for security.
 */
/** SQL operation error types */
type sqlError =
  | InvalidIdentifier
  | InvalidParameterIndex
  | TooManyParameters
  | EmptyQuery
  | UnbalancedQuotes
  | DangerousKeyword

/** SQL value types for parameterized queries */
type valueType =
  | Null
  | Integer
  | Float
  | Text
  | Blob
  | Boolean

/** SQL parameter value */
type sqlValue =
  | SqlNull
  | SqlInteger(int)
  | SqlFloat(float)
  | SqlText(string)
  | SqlBlob(array<int>)
  | SqlBoolean(bool)

/** Placeholder style for different database systems */
type placeholderStyle =
  | QuestionMark // ? (SQLite, MySQL)
  | DollarNumber // $1, $2 (PostgreSQL)
  | AtName // @name (SQL Server)
  | ColonNumber // :1, :2 (Oracle)

/** Maximum number of parameters allowed in a single query */
let maxParameters = 1000

/** Dangerous SQL keywords that should trigger warnings */
let dangerousKeywords = [
  "DROP",
  "DELETE",
  "TRUNCATE",
  "ALTER",
  "GRANT",
  "REVOKE",
  "EXEC",
  "EXECUTE",
  "SHUTDOWN",
  "CREATE USER",
  "DROP USER",
]

/** Escape a string for safe SQL string literal insertion.
 * Doubles single quotes and handles special characters.
 */
let escapeStringLiteral = (value: string): string => {
  let escaped =
    value
    ->Js.String2.replaceByRe(%re("/'/g"), "''")
    ->Js.String2.replaceByRe(%re("/\\/g"), "\\\\")
    ->Js.String2.replaceByRe(%re("/\x00/g"), "\\0")
    ->Js.String2.replaceByRe(%re("/\n/g"), "\\n")
    ->Js.String2.replaceByRe(%re("/\r/g"), "\\r")
    ->Js.String2.replaceByRe(%re("/\x1a/g"), "\\Z")
  `'${escaped}'`
}

/** Convert a byte to hex string */
let byteToHex = (byte: int): string => {
  let hexChars = "0123456789ABCDEF"
  let high = Js.String2.charAt(hexChars, land(lsr(byte, 4), 0x0F))
  let low = Js.String2.charAt(hexChars, land(byte, 0x0F))
  high ++ low
}

/** Escape a blob/binary value as hex literal */
let escapeBlobLiteral = (value: array<int>): string => {
  let hexParts = Js.Array2.map(value, byteToHex)
  `X'${Js.Array2.joinWith(hexParts, "")}'`
}

/** Check if first character is valid for identifier (letter or underscore) */
let isValidIdentifierFirstChar = (c: string): bool => {
  let code = Js.String2.charCodeAt(c, 0)
  (code >= 65.0 && code <= 90.0) ||
  // A-Z
  code >= 97.0 && code <= 122.0 ||
  // a-z
  code == 95.0 // _
}

/** Check if character is valid for identifier (alphanumeric or underscore) */
let isValidIdentifierChar = (c: string): bool => {
  let code = Js.String2.charCodeAt(c, 0)
  (code >= 48.0 && code <= 57.0) ||
  // 0-9
  code >= 65.0 && code <= 90.0 ||
  // A-Z
  code >= 97.0 && code <= 122.0 ||
  // a-z
  code == 95.0 // _
}

/** Validate and escape a SQL identifier (table name, column name).
 * Only allows alphanumeric characters and underscores.
 */
let escapeIdentifier = (identifier: string): result<string, sqlError> => {
  let length = Js.String2.length(identifier)
  if length == 0 {
    Error(InvalidIdentifier)
  } else {
    let firstChar = Js.String2.charAt(identifier, 0)
    if !isValidIdentifierFirstChar(firstChar) {
      Error(InvalidIdentifier)
    } else {
      let chars = Js.String2.split(identifier, "")
      let allValid = Js.Array2.every(chars, isValidIdentifierChar)
      if allValid {
        Ok(`"${identifier}"`)
      } else {
        Error(InvalidIdentifier)
      }
    }
  }
}

/** Check if a string contains dangerous SQL keywords */
let containsDangerousKeyword = (query: string): bool => {
  let upper = Js.String2.toUpperCase(query)
  Js.Array2.some(dangerousKeywords, keyword => Js.String2.includes(upper, keyword))
}

/** Check if a query string has balanced quotes */
let hasBalancedQuotes = (query: string): bool => {
  let length = Js.String2.length(query)
  let singleQuoteCount = ref(0)
  let doubleQuoteCount = ref(0)
  let inSingle = ref(false)
  let inDouble = ref(false)
  let prevChar = ref("")

  for i in 0 to length - 1 {
    let c = Js.String2.charAt(query, i)
    if c == "'" && prevChar.contents != "\\" && !inDouble.contents {
      inSingle := !inSingle.contents
      singleQuoteCount := singleQuoteCount.contents + 1
    } else if c == "\"" && prevChar.contents != "\\" && !inSingle.contents {
      inDouble := !inDouble.contents
      doubleQuoteCount := doubleQuoteCount.contents + 1
    }
    prevChar := c
  }

  mod(singleQuoteCount.contents, 2) == 0 && mod(doubleQuoteCount.contents, 2) == 0
}

/** Sanitize a value for use in a LIKE pattern.
 * Escapes %, _, and \ characters.
 */
let escapeLikePattern = (pattern: string, escapeChar: string): string => {
  let result = ref("")
  let length = Js.String2.length(pattern)

  for i in 0 to length - 1 {
    let c = Js.String2.charAt(pattern, i)
    if c == "%" || c == "_" || c == escapeChar {
      result := result.contents ++ escapeChar
    }
    result := result.contents ++ c
  }

  result.contents
}

/** Validate that a query is safe for execution (basic checks) */
let validateQuery = (query: string): result<unit, sqlError> => {
  if Js.String2.length(query) == 0 {
    Error(EmptyQuery)
  } else if !hasBalancedQuotes(query) {
    Error(UnbalancedQuotes)
  } else {
    Ok()
  }
}

/** Check if value is SQL null */
let isNull = (value: sqlValue): bool => {
  switch value {
  | SqlNull => true
  | _ => false
  }
}

/** Convert a SQL value to a SQL literal string */
let toSqlLiteral = (value: sqlValue): string => {
  switch value {
  | SqlNull => "NULL"
  | SqlInteger(i) => Belt.Int.toString(i)
  | SqlFloat(f) => Belt.Float.toString(f)
  | SqlText(t) => escapeStringLiteral(t)
  | SqlBlob(b) => escapeBlobLiteral(b)
  | SqlBoolean(b) => b ? "TRUE" : "FALSE"
  }
}

/** Query builder state */
type queryBuilder = {
  mutable queryParts: array<string>,
  mutable parameters: array<sqlValue>,
  placeholderStyle: placeholderStyle,
}

/** Create a new query builder */
let createQueryBuilder = (style: placeholderStyle): queryBuilder => {
  {queryParts: [], parameters: [], placeholderStyle: style}
}

/** Add a literal SQL fragment to the builder */
let addLiteral = (builder: queryBuilder, sql: string): unit => {
  builder.queryParts = Js.Array2.concat(builder.queryParts, [sql])
}

/** Add a parameter placeholder and value to the builder */
let addParam = (builder: queryBuilder, value: sqlValue): result<unit, sqlError> => {
  if Js.Array2.length(builder.parameters) >= maxParameters {
    Error(TooManyParameters)
  } else {
    builder.parameters = Js.Array2.concat(builder.parameters, [value])
    builder.queryParts = Js.Array2.concat(builder.queryParts, ["\x00"]) // Marker
    Ok()
  }
}

/** Build the final query string with placeholders */
let buildQuery = (builder: queryBuilder): string => {
  let result = ref("")
  let paramIndex = ref(1)

  Js.Array2.forEach(builder.queryParts, part => {
    if part == "\x00" {
      // Replace marker with placeholder
      let placeholder = switch builder.placeholderStyle {
      | QuestionMark => "?"
      | DollarNumber => `$${Belt.Int.toString(paramIndex.contents)}`
      | AtName => `@p${Belt.Int.toString(paramIndex.contents)}`
      | ColonNumber => `:${Belt.Int.toString(paramIndex.contents)}`
      }
      result := result.contents ++ placeholder
      paramIndex := paramIndex.contents + 1
    } else {
      result := result.contents ++ part
    }
  })

  result.contents
}

/** Get the parameters list from the builder */
let getParameters = (builder: queryBuilder): array<sqlValue> => {
  builder.parameters
}

/** Strip SQL comments from a query (both -- and /* */ style) */
let stripComments = (query: string): string => {
  let result = ref("")
  let length = Js.String2.length(query)
  let i = ref(0)
  let inString = ref(false)
  let stringChar = ref("")

  while i.contents < length {
    let c = Js.String2.charAt(query, i.contents)

    // Track string literals to avoid stripping inside them
    if !inString.contents && (c == "'" || c == "\"") {
      inString := true
      stringChar := c
      result := result.contents ++ c
      i := i.contents + 1
    } else if inString.contents {
      result := result.contents ++ c
      if (
        c == stringChar.contents &&
          (i.contents == 0 || Js.String2.charAt(query, i.contents - 1) != "\\")
      ) {
        inString := false
      }
      i := i.contents + 1
    } else if (
      // Check for -- comment
      i.contents + 1 < length && c == "-" && Js.String2.charAt(query, i.contents + 1) == "-"
    ) {
      // Skip until end of line
      while i.contents < length && Js.String2.charAt(query, i.contents) != "\n" {
        i := i.contents + 1
      }
    } else if (
      // Check for /* */ comment
      i.contents + 1 < length && c == "/" && Js.String2.charAt(query, i.contents + 1) == "*"
    ) {
      i := i.contents + 2
      let foundClose = ref(false)
      while i.contents + 1 < length && !foundClose.contents {
        if (
          Js.String2.charAt(query, i.contents) == "*" &&
            Js.String2.charAt(query, i.contents + 1) == "/"
        ) {
          i := i.contents + 2
          foundClose := true
        } else {
          i := i.contents + 1
        }
      }
    } else {
      result := result.contents ++ c
      i := i.contents + 1
    }
  }

  result.contents
}

/** Create a simple SELECT query with safe identifiers */
let selectQuery = (table: string, columns: array<string>, whereClause: option<string>): result<
  string,
  sqlError,
> => {
  switch escapeIdentifier(table) {
  | Error(e) => Error(e)
  | Ok(escapedTable) =>
    let columnResults = Js.Array2.map(columns, escapeIdentifier)
    let hasError = Js.Array2.some(columnResults, r =>
      switch r {
      | Error(_) => true
      | Ok(_) => false
      }
    )

    if hasError {
      Error(InvalidIdentifier)
    } else {
      let escapedColumns = Js.Array2.map(columnResults, r =>
        switch r {
        | Ok(c) => c
        | Error(_) => ""
        }
      )

      let columnsStr =
        Js.Array2.length(escapedColumns) == 0 ? "*" : Js.Array2.joinWith(escapedColumns, ", ")

      let query = `SELECT ${columnsStr} FROM ${escapedTable}`

      let finalQuery = switch whereClause {
      | Some(clause) => `${query} WHERE ${clause}`
      | None => query
      }

      Ok(finalQuery)
    }
  }
}

/** Create a parameterized INSERT query */
let insertQuery = (table: string, columns: array<string>, style: placeholderStyle): result<
  string,
  sqlError,
> => {
  switch escapeIdentifier(table) {
  | Error(e) => Error(e)
  | Ok(escapedTable) =>
    let columnResults = Js.Array2.map(columns, escapeIdentifier)
    let hasError = Js.Array2.some(columnResults, r =>
      switch r {
      | Error(_) => true
      | Ok(_) => false
      }
    )

    if hasError {
      Error(InvalidIdentifier)
    } else if Js.Array2.length(columns) == 0 {
      Error(EmptyQuery)
    } else {
      let escapedColumns = Js.Array2.map(columnResults, r =>
        switch r {
        | Ok(c) => c
        | Error(_) => ""
        }
      )

      let columnsStr = Js.Array2.joinWith(escapedColumns, ", ")
      let numCols = Js.Array2.length(columns)

      let placeholders = switch style {
      | QuestionMark => Js.Array2.joinWith(Belt.Array.make(numCols, "?"), ", ")
      | DollarNumber =>
        Js.Array2.joinWith(
          Belt.Array.mapWithIndex(Belt.Array.make(numCols, ""), (i, _) =>
            `$${Belt.Int.toString(i + 1)}`
          ),
          ", ",
        )
      | AtName =>
        Js.Array2.joinWith(
          Belt.Array.mapWithIndex(Belt.Array.make(numCols, ""), (i, _) =>
            `@p${Belt.Int.toString(i + 1)}`
          ),
          ", ",
        )
      | ColonNumber =>
        Js.Array2.joinWith(
          Belt.Array.mapWithIndex(Belt.Array.make(numCols, ""), (i, _) =>
            `:${Belt.Int.toString(i + 1)}`
          ),
          ", ",
        )
      }

      Ok(`INSERT INTO ${escapedTable} (${columnsStr}) VALUES (${placeholders})`)
    }
  }
}

/** Create a parameterized UPDATE query */
let updateQuery = (
  table: string,
  columns: array<string>,
  whereClause: string,
  style: placeholderStyle,
): result<string, sqlError> => {
  switch escapeIdentifier(table) {
  | Error(e) => Error(e)
  | Ok(escapedTable) =>
    if Js.Array2.length(columns) == 0 {
      Error(EmptyQuery)
    } else {
      let setParts = ref([])
      let hasError = ref(false)
      let paramIndex = ref(1)

      Js.Array2.forEach(columns, col => {
        switch escapeIdentifier(col) {
        | Error(_) => hasError := true
        | Ok(escapedCol) =>
          let placeholder = switch style {
          | QuestionMark => "?"
          | DollarNumber => `$${Belt.Int.toString(paramIndex.contents)}`
          | AtName => `@p${Belt.Int.toString(paramIndex.contents)}`
          | ColonNumber => `:${Belt.Int.toString(paramIndex.contents)}`
          }
          setParts := Js.Array2.concat(setParts.contents, [`${escapedCol} = ${placeholder}`])
          paramIndex := paramIndex.contents + 1
        }
      })

      if hasError.contents {
        Error(InvalidIdentifier)
      } else {
        let setStr = Js.Array2.joinWith(setParts.contents, ", ")
        Ok(`UPDATE ${escapedTable} SET ${setStr} WHERE ${whereClause}`)
      }
    }
  }
}

/** Check if a query is safe (no dangerous keywords, balanced quotes) */
let isSafeQuery = (query: string): bool => {
  hasBalancedQuotes(query) && !containsDangerousKeyword(query)
}

/** Warn if query contains dangerous keywords */
let warnIfDangerous = (query: string): option<string> => {
  if containsDangerousKeyword(query) {
    Some("Query contains potentially dangerous SQL keywords")
  } else {
    None
  }
}
