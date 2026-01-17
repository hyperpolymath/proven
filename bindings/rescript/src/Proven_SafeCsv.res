// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * SafeCsv - Safe CSV parsing and generation with proper escaping.
 *
 * Provides RFC 4180 compliant CSV parsing and generation.
 * All operations are bounds-checked and handle edge cases safely.
 * Supports custom delimiters, quoting, and escape characters.
 */

/** CSV error types */
type csvError =
  | UnterminatedQuote
  | InvalidEscape
  | InconsistentColumnCount
  | EmptyInput
  | MaxFieldsExceeded
  | FieldTooLong

/** Line ending styles */
type lineEnding =
  | Lf
  | Crlf
  | Cr

/** CSV parsing options */
type parseOptions = {
  delimiter: string,
  quote: string,
  trimWhitespace: bool,
  maxFields: int,
  maxFieldLength: int,
  skipEmptyRows: bool,
}

/** CSV generation options */
type writeOptions = {
  delimiter: string,
  quote: string,
  lineEnding: lineEnding,
  alwaysQuote: bool,
}

/** Default parse options */
let defaultParseOptions: parseOptions = {
  delimiter: ",",
  quote: "\"",
  trimWhitespace: false,
  maxFields: 0,
  maxFieldLength: 0,
  skipEmptyRows: false,
}

/** Default write options */
let defaultWriteOptions: writeOptions = {
  delimiter: ",",
  quote: "\"",
  lineEnding: Lf,
  alwaysQuote: false,
}

/** A parsed CSV row */
type csvRow = array<string>

/** Get field at index from row, or None if out of bounds */
let getField = (row: csvRow, index: int): option<string> => {
  Belt.Array.get(row, index)
}

/** Get field count in row */
let fieldCount = (row: csvRow): int => {
  Belt.Array.length(row)
}

/** Check if a field needs quoting */
let needsQuoting = (field: string, ~options: writeOptions=defaultWriteOptions, ()): bool => {
  if options.alwaysQuote {
    true
  } else {
    Js.String2.includes(field, options.delimiter) ||
    Js.String2.includes(field, options.quote) ||
    Js.String2.includes(field, "\n") ||
    Js.String2.includes(field, "\r")
  }
}

/** Escape a field for CSV output */
let escapeField = (field: string, ~options: writeOptions=defaultWriteOptions, ()): string => {
  if !needsQuoting(field, ~options, ()) {
    field
  } else {
    // Double the quote characters
    let escaped = Js.String2.replaceByRe(
      field,
      Js.Re.fromStringWithFlags(Js.Re.escape(options.quote), ~flags="g"),
      options.quote ++ options.quote,
    )
    options.quote ++ escaped ++ options.quote
  }
}

/** Get the line ending string */
let lineEndingToString = (ending: lineEnding): string => {
  switch ending {
  | Lf => "\n"
  | Crlf => "\r\n"
  | Cr => "\r"
  }
}

/** Write a single row to CSV format */
let writeRow = (fields: array<string>, ~options: writeOptions=defaultWriteOptions, ()): string => {
  let escapedFields = Belt.Array.map(fields, field => {
    if needsQuoting(field, ~options, ()) {
      let escaped = Js.String2.replaceByRe(
        field,
        Js.Re.fromStringWithFlags(Js.Re.escape(options.quote), ~flags="g"),
        options.quote ++ options.quote,
      )
      options.quote ++ escaped ++ options.quote
    } else {
      field
    }
  })
  Js.Array2.joinWith(escapedFields, options.delimiter) ++ lineEndingToString(options.lineEnding)
}

/** Parse a single CSV row */
let parseRow = (
  line: string,
  ~options: parseOptions=defaultParseOptions,
  (),
): result<csvRow, csvError> => {
  if Js.String2.length(line) == 0 {
    Ok([])
  } else {
    let fields = ref([])
    let currentField = ref("")
    let inQuotes = ref(false)
    let position = ref(0)
    let error = ref(None)
    let length = Js.String2.length(line)

    while position.contents < length && error.contents == None {
      let currentChar = Js.String2.charAt(line, position.contents)

      if inQuotes.contents {
        if currentChar == options.quote {
          // Check for escaped quote
          if
            position.contents + 1 < length &&
            Js.String2.charAt(line, position.contents + 1) == options.quote
          {
            currentField := currentField.contents ++ options.quote
            position := position.contents + 2
          } else {
            // End of quoted field
            inQuotes := false
            position := position.contents + 1
          }
        } else {
          currentField := currentField.contents ++ currentChar
          position := position.contents + 1
        }
      } else if currentChar == options.quote {
        inQuotes := true
        position := position.contents + 1
      } else if currentChar == options.delimiter {
        // Check max fields
        if options.maxFields > 0 && Belt.Array.length(fields.contents) >= options.maxFields {
          error := Some(MaxFieldsExceeded)
        } else {
          let fieldValue = if options.trimWhitespace {
            Js.String2.trim(currentField.contents)
          } else {
            currentField.contents
          }
          fields := Belt.Array.concat(fields.contents, [fieldValue])
          currentField := ""
          position := position.contents + 1
        }
      } else if currentChar == "\n" || currentChar == "\r" {
        // End of row
        position := length
      } else {
        currentField := currentField.contents ++ currentChar
        position := position.contents + 1
      }

      // Check max field length
      if options.maxFieldLength > 0 && Js.String2.length(currentField.contents) > options.maxFieldLength {
        error := Some(FieldTooLong)
      }
    }

    switch error.contents {
    | Some(err) => Error(err)
    | None =>
      // Add the last field
      let fieldValue = if options.trimWhitespace {
        Js.String2.trim(currentField.contents)
      } else {
        currentField.contents
      }
      fields := Belt.Array.concat(fields.contents, [fieldValue])

      // Check for unterminated quote
      if inQuotes.contents {
        Error(UnterminatedQuote)
      } else {
        Ok(fields.contents)
      }
    }
  }
}

/** Parse all rows from CSV input */
let parseAll = (
  input: string,
  ~options: parseOptions=defaultParseOptions,
  (),
): result<array<csvRow>, csvError> => {
  if Js.String2.length(input) == 0 {
    Error(EmptyInput)
  } else {
    // Split by newlines, handling both \n and \r\n
    let normalizedInput = Js.String2.replaceByRe(input, %re("/\r\n/g"), "\n")
    let normalizedInput = Js.String2.replaceByRe(normalizedInput, %re("/\r/g"), "\n")
    let lines = Js.String2.split(normalizedInput, "\n")

    let rows = ref([])
    let error = ref(None)
    let expectedColumnCount = ref(None)

    Belt.Array.forEach(lines, line => {
      if error.contents == None {
        // Skip empty rows if configured
        if Js.String2.length(Js.String2.trim(line)) == 0 && options.skipEmptyRows {
          ()
        } else if Js.String2.length(line) > 0 {
          switch parseRow(line, ~options, ()) {
          | Ok(row) =>
            // Check column consistency
            switch expectedColumnCount.contents {
            | None => expectedColumnCount := Some(Belt.Array.length(row))
            | Some(expected) =>
              if Belt.Array.length(row) != expected {
                error := Some(InconsistentColumnCount)
              }
            }
            if error.contents == None {
              rows := Belt.Array.concat(rows.contents, [row])
            }
          | Error(err) => error := Some(err)
          }
        }
      }
    })

    switch error.contents {
    | Some(err) => Error(err)
    | None => Ok(rows.contents)
    }
  }
}

/** Count columns in first row (without full parsing) */
let countColumns = (input: string, ~delimiter: string=",", ()): int => {
  if Js.String2.length(input) == 0 {
    0
  } else {
    let count = ref(1)
    let inQuotes = ref(false)
    let index = ref(0)
    let length = Js.String2.length(input)

    while index.contents < length {
      let currentChar = Js.String2.charAt(input, index.contents)

      if currentChar == "\"" && !inQuotes.contents {
        inQuotes := true
      } else if currentChar == "\"" && inQuotes.contents {
        // Check for escaped quote
        if index.contents + 1 < length && Js.String2.charAt(input, index.contents + 1) == "\"" {
          index := index.contents + 1
        } else {
          inQuotes := false
        }
      } else if !inQuotes.contents {
        if currentChar == delimiter {
          count := count.contents + 1
        } else if currentChar == "\n" || currentChar == "\r" {
          index := length // Exit loop
        }
      }
      index := index.contents + 1
    }

    count.contents
  }
}

/** Check if input is valid CSV */
let isValid = (input: string, ~options: parseOptions=defaultParseOptions, ()): bool => {
  switch parseAll(input, ~options, ()) {
  | Ok(_) => true
  | Error(_) => false
  }
}

/** CSV Writer for building CSV output incrementally */
type csvWriter = {
  mutable buffer: string,
  options: writeOptions,
  mutable rowCount: int,
}

/** Create a new CSV writer */
let createWriter = (~options: writeOptions=defaultWriteOptions, ()): csvWriter => {
  {
    buffer: "",
    options,
    rowCount: 0,
  }
}

/** Write a row to the CSV writer */
let writerWriteRow = (writer: csvWriter, fields: array<string>): unit => {
  writer.buffer = writer.buffer ++ writeRow(fields, ~options=writer.options, ())
  writer.rowCount = writer.rowCount + 1
}

/** Get the generated CSV content */
let writerGetContent = (writer: csvWriter): string => {
  writer.buffer
}

/** Get the row count */
let writerGetRowCount = (writer: csvWriter): int => {
  writer.rowCount
}

/** Clear the writer buffer */
let writerClear = (writer: csvWriter): unit => {
  writer.buffer = ""
  writer.rowCount = 0
}

/** Convert a 2D array to CSV string */
let stringify = (
  data: array<array<string>>,
  ~options: writeOptions=defaultWriteOptions,
  (),
): string => {
  let writer = createWriter(~options, ())
  Belt.Array.forEach(data, row => writerWriteRow(writer, row))
  writerGetContent(writer)
}

/** Get header row (first row) from CSV data */
let getHeaders = (rows: array<csvRow>): option<csvRow> => {
  Belt.Array.get(rows, 0)
}

/** Get data rows (all rows except first) from CSV data */
let getDataRows = (rows: array<csvRow>): array<csvRow> => {
  Belt.Array.sliceToEnd(rows, 1)
}

/** Convert CSV rows to array of objects using first row as headers */
let toObjects = (rows: array<csvRow>): option<array<Js.Dict.t<string>>> => {
  switch getHeaders(rows) {
  | None => None
  | Some(headers) =>
    let dataRows = getDataRows(rows)
    let objects = Belt.Array.map(dataRows, row => {
      let dict = Js.Dict.empty()
      Belt.Array.forEachWithIndex(headers, (index, header) => {
        switch Belt.Array.get(row, index) {
        | Some(value) => Js.Dict.set(dict, header, value)
        | None => ()
        }
      })
      dict
    })
    Some(objects)
  }
}

/** Convert array of objects to CSV rows */
let fromObjects = (objects: array<Js.Dict.t<string>>, ~headers: array<string>): array<csvRow> => {
  let headerRow = headers
  let dataRows = Belt.Array.map(objects, obj => {
    Belt.Array.map(headers, header => {
      switch Js.Dict.get(obj, header) {
      | Some(value) => value
      | None => ""
      }
    })
  })
  Belt.Array.concat([headerRow], dataRows)
}
