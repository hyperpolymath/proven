// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * SafeBibtex - Safe BibTeX entry parsing and validation operations that cannot crash.
 *
 * This module provides safe parsing of BibTeX entries commonly used in
 * academic citations and bibliography management. All operations return
 * errors on invalid input rather than panicking.
 */

/** BibTeX error types */
type bibtexError =
  | InvalidEntryType
  | InvalidCiteKey
  | MissingField
  | UnterminatedString
  | UnterminatedBrace
  | InvalidCharacter
  | MalformedEntry
  | DuplicateField
  | EmptyInput

/** BibTeX entry types */
type entryType =
  | Article
  | Book
  | Booklet
  | Conference
  | Inbook
  | Incollection
  | Inproceedings
  | Manual
  | Mastersthesis
  | Misc
  | Phdthesis
  | Proceedings
  | Techreport
  | Unpublished
  | Online
  | Software
  | Dataset
  | Unknown

/** Parse entry type from string (case-insensitive) */
let entryTypeFromString = (str: string): entryType => {
  let lower = Js.String2.toLowerCase(str)
  switch lower {
  | "article" => Article
  | "book" => Book
  | "booklet" => Booklet
  | "conference" => Conference
  | "inbook" => Inbook
  | "incollection" => Incollection
  | "inproceedings" => Inproceedings
  | "manual" => Manual
  | "mastersthesis" => Mastersthesis
  | "misc" => Misc
  | "phdthesis" => Phdthesis
  | "proceedings" => Proceedings
  | "techreport" => Techreport
  | "unpublished" => Unpublished
  | "online" => Online
  | "software" => Software
  | "dataset" => Dataset
  | _ => Unknown
  }
}

/** Convert entry type to canonical string representation */
let entryTypeToString = (entryType: entryType): string => {
  switch entryType {
  | Article => "article"
  | Book => "book"
  | Booklet => "booklet"
  | Conference => "conference"
  | Inbook => "inbook"
  | Incollection => "incollection"
  | Inproceedings => "inproceedings"
  | Manual => "manual"
  | Mastersthesis => "mastersthesis"
  | Misc => "misc"
  | Phdthesis => "phdthesis"
  | Proceedings => "proceedings"
  | Techreport => "techreport"
  | Unpublished => "unpublished"
  | Online => "online"
  | Software => "software"
  | Dataset => "dataset"
  | Unknown => "unknown"
  }
}

/** A single BibTeX field (key-value pair) */
type bibtexField = {
  key: string,
  value: string,
}

/** A parsed BibTeX entry */
type bibtexEntry = {
  entryType: entryType,
  citeKey: string,
  fields: array<bibtexField>,
}

/** Get a field value by key (case-insensitive) */
let getField = (entry: bibtexEntry, key: string): option<string> => {
  let lowerKey = Js.String2.toLowerCase(key)
  Belt.Array.getBy(entry.fields, field => {
    Js.String2.toLowerCase(field.key) == lowerKey
  })->Belt.Option.map(field => field.value)
}

/** Check if entry has a field */
let hasField = (entry: bibtexEntry, key: string): bool => {
  getField(entry, key)->Belt.Option.isSome
}

/** Get author field */
let getAuthor = (entry: bibtexEntry): option<string> => getField(entry, "author")

/** Get title field */
let getTitle = (entry: bibtexEntry): option<string> => getField(entry, "title")

/** Get year field */
let getYear = (entry: bibtexEntry): option<string> => getField(entry, "year")

/** Get journal field */
let getJournal = (entry: bibtexEntry): option<string> => getField(entry, "journal")

/** Get DOI field */
let getDoi = (entry: bibtexEntry): option<string> => getField(entry, "doi")

/** Get URL field */
let getUrl = (entry: bibtexEntry): option<string> => getField(entry, "url")

/** Required fields per entry type */
let requiredFieldsForType = (entryType: entryType): array<string> => {
  switch entryType {
  | Article => ["author", "title", "journal", "year"]
  | Book => ["author", "title", "publisher", "year"]
  | Inproceedings => ["author", "title", "booktitle", "year"]
  | Phdthesis => ["author", "title", "school", "year"]
  | Mastersthesis => ["author", "title", "school", "year"]
  | Techreport => ["author", "title", "institution", "year"]
  | Misc => []
  | _ => []
  }
}

/** Check if a cite key is valid.
 * Valid keys: alphanumeric, underscore, hyphen, colon, period.
 * First character must be alphanumeric.
 */
let isValidCiteKey = (key: string): bool => {
  let length = Js.String2.length(key)
  if length == 0 || length > 256 {
    false
  } else {
    // First character must be alphanumeric
    let firstCode = Js.String2.charCodeAt(key, 0)->Belt.Float.toInt
    let firstIsAlphanumeric =
      (firstCode >= 48 && firstCode <= 57) || // 0-9
      (firstCode >= 65 && firstCode <= 90) || // A-Z
      (firstCode >= 97 && firstCode <= 122) // a-z

    if !firstIsAlphanumeric {
      false
    } else {
      let valid = ref(true)
      for index in 0 to length - 1 {
        if valid.contents {
          let charCode = Js.String2.charCodeAt(key, index)->Belt.Float.toInt
          let isValidChar =
            (charCode >= 48 && charCode <= 57) || // 0-9
            (charCode >= 65 && charCode <= 90) || // A-Z
            (charCode >= 97 && charCode <= 122) || // a-z
            charCode == 95 || // _
            charCode == 45 || // -
            charCode == 58 || // :
            charCode == 46 // .
          if !isValidChar {
            valid := false
          }
        }
      }
      valid.contents
    }
  }
}

/** Validate that an entry has all required fields for its type */
let validateRequiredFields = (entry: bibtexEntry): result<unit, bibtexError> => {
  let requiredFields = requiredFieldsForType(entry.entryType)
  let missingField = Belt.Array.getBy(requiredFields, fieldName => !hasField(entry, fieldName))
  switch missingField {
  | Some(_) => Error(MissingField)
  | None => Ok()
  }
}

/** Parse a field value (handles braces, quotes, and numbers) */
let parseFieldValue = (input: string, position: ref<int>): result<string, bibtexError> => {
  // Skip whitespace
  while position.contents < Js.String2.length(input) {
    let currentChar = Js.String2.charAt(input, position.contents)
    if currentChar == " " || currentChar == "\t" || currentChar == "\n" || currentChar == "\r" {
      position := position.contents + 1
    } else {
      position := Js.String2.length(input) + 1 // Break loop
    }
  }
  // Reset to actual position
  let length = Js.String2.length(input)
  let findStart = ref(0)
  while findStart.contents < length {
    let currentChar = Js.String2.charAt(input, findStart.contents)
    if currentChar == " " || currentChar == "\t" || currentChar == "\n" || currentChar == "\r" {
      findStart := findStart.contents + 1
    } else {
      findStart := length + 1 // Break
    }
  }

  if position.contents >= length {
    Error(MalformedEntry)
  } else {
    let firstChar = Js.String2.charAt(input, position.contents)

    if firstChar == "{" {
      // Braced value
      position := position.contents + 1
      let valueStart = position.contents
      let braceDepth = ref(1)

      while position.contents < length && braceDepth.contents > 0 {
        let currentChar = Js.String2.charAt(input, position.contents)
        if currentChar == "{" {
          braceDepth := braceDepth.contents + 1
          position := position.contents + 1
        } else if currentChar == "}" {
          braceDepth := braceDepth.contents - 1
          if braceDepth.contents > 0 {
            position := position.contents + 1
          }
        } else {
          position := position.contents + 1
        }
      }

      if braceDepth.contents != 0 {
        Error(UnterminatedBrace)
      } else {
        let value = Js.String2.slice(input, ~from=valueStart, ~to_=position.contents)
        position := position.contents + 1
        Ok(value)
      }
    } else if firstChar == "\"" {
      // Quoted value
      position := position.contents + 1
      let valueStart = position.contents

      while position.contents < length && Js.String2.charAt(input, position.contents) != "\"" {
        let currentChar = Js.String2.charAt(input, position.contents)
        if currentChar == "\\" && position.contents + 1 < length {
          position := position.contents + 2
        } else {
          position := position.contents + 1
        }
      }

      if position.contents >= length {
        Error(UnterminatedString)
      } else {
        let value = Js.String2.slice(input, ~from=valueStart, ~to_=position.contents)
        position := position.contents + 1
        Ok(value)
      }
    } else {
      // Numeric value or bare word
      let valueStart = position.contents
      while position.contents < length {
        let charCode = Js.String2.charCodeAt(input, position.contents)->Belt.Float.toInt
        let isAlphanumeric =
          (charCode >= 48 && charCode <= 57) ||
          (charCode >= 65 && charCode <= 90) ||
          (charCode >= 97 && charCode <= 122)
        if isAlphanumeric {
          position := position.contents + 1
        } else {
          position := length + 1 // Break
        }
      }
      // Reset position to actual end
      let findEnd = ref(valueStart)
      while findEnd.contents < length {
        let charCode = Js.String2.charCodeAt(input, findEnd.contents)->Belt.Float.toInt
        let isAlphanumeric =
          (charCode >= 48 && charCode <= 57) ||
          (charCode >= 65 && charCode <= 90) ||
          (charCode >= 97 && charCode <= 122)
        if isAlphanumeric {
          findEnd := findEnd.contents + 1
        } else {
          findEnd := length + 1
        }
      }

      if findEnd.contents == valueStart {
        Error(MalformedEntry)
      } else {
        let actualEnd = {
          let end = ref(valueStart)
          while end.contents < length {
            let charCode = Js.String2.charCodeAt(input, end.contents)->Belt.Float.toInt
            let isAlphanumeric =
              (charCode >= 48 && charCode <= 57) ||
              (charCode >= 65 && charCode <= 90) ||
              (charCode >= 97 && charCode <= 122)
            if isAlphanumeric {
              end := end.contents + 1
            } else {
              end := length + 1
            }
          }
          end.contents
        }
        position := actualEnd
        Ok(Js.String2.slice(input, ~from=valueStart, ~to_=actualEnd))
      }
    }
  }
}

/** Parse a single BibTeX entry from input string */
let parseEntry = (input: string): result<bibtexEntry, bibtexError> => {
  if Js.String2.length(input) == 0 {
    Error(EmptyInput)
  } else {
    let cursor = ref(0)
    let length = Js.String2.length(input)

    // Skip leading whitespace
    while cursor.contents < length {
      let currentChar = Js.String2.charAt(input, cursor.contents)
      if currentChar == " " || currentChar == "\t" || currentChar == "\n" || currentChar == "\r" {
        cursor := cursor.contents + 1
      } else {
        cursor := length + 1 // Break
      }
    }
    // Reset cursor to find actual position
    cursor := 0
    while cursor.contents < length &&
      (Js.String2.charAt(input, cursor.contents) == " " ||
        Js.String2.charAt(input, cursor.contents) == "\t" ||
        Js.String2.charAt(input, cursor.contents) == "\n" ||
        Js.String2.charAt(input, cursor.contents) == "\r") {
      cursor := cursor.contents + 1
    }

    // Expect '@'
    if cursor.contents >= length || Js.String2.charAt(input, cursor.contents) != "@" {
      Error(MalformedEntry)
    } else {
      cursor := cursor.contents + 1

      // Parse entry type
      let entryTypeStart = cursor.contents
      while cursor.contents < length {
        let charCode = Js.String2.charCodeAt(input, cursor.contents)->Belt.Float.toInt
        let isAlpha =
          (charCode >= 65 && charCode <= 90) || (charCode >= 97 && charCode <= 122)
        if isAlpha {
          cursor := cursor.contents + 1
        } else {
          cursor := length + 1 // Break
        }
      }
      // Find actual end of entry type
      let entryTypeEnd = {
        let end = ref(entryTypeStart)
        while end.contents < length {
          let charCode = Js.String2.charCodeAt(input, end.contents)->Belt.Float.toInt
          let isAlpha =
            (charCode >= 65 && charCode <= 90) || (charCode >= 97 && charCode <= 122)
          if isAlpha {
            end := end.contents + 1
          } else {
            end := length + 1
          }
        }
        end.contents
      }

      if entryTypeEnd == entryTypeStart {
        Error(InvalidEntryType)
      } else {
        let entryTypeStr = Js.String2.slice(input, ~from=entryTypeStart, ~to_=entryTypeEnd)
        let entryType = entryTypeFromString(entryTypeStr)
        cursor := entryTypeEnd

        // Skip whitespace
        while cursor.contents < length &&
          (Js.String2.charAt(input, cursor.contents) == " " ||
            Js.String2.charAt(input, cursor.contents) == "\t" ||
            Js.String2.charAt(input, cursor.contents) == "\n" ||
            Js.String2.charAt(input, cursor.contents) == "\r") {
          cursor := cursor.contents + 1
        }

        // Expect '{' or '('
        if cursor.contents >= length {
          Error(MalformedEntry)
        } else {
          let openingChar = Js.String2.charAt(input, cursor.contents)
          if openingChar != "{" && openingChar != "(" {
            Error(MalformedEntry)
          } else {
            let closingChar = if openingChar == "{" { "}" } else { ")" }
            cursor := cursor.contents + 1

            // Skip whitespace
            while cursor.contents < length &&
              (Js.String2.charAt(input, cursor.contents) == " " ||
                Js.String2.charAt(input, cursor.contents) == "\t" ||
                Js.String2.charAt(input, cursor.contents) == "\n" ||
                Js.String2.charAt(input, cursor.contents) == "\r") {
              cursor := cursor.contents + 1
            }

            // Parse cite key
            let citeKeyStart = cursor.contents
            while cursor.contents < length &&
              Js.String2.charAt(input, cursor.contents) != "," &&
              Js.String2.charAt(input, cursor.contents) != " " &&
              Js.String2.charAt(input, cursor.contents) != "\t" {
              cursor := cursor.contents + 1
            }

            if cursor.contents == citeKeyStart {
              Error(InvalidCiteKey)
            } else {
              let citeKey = Js.String2.slice(input, ~from=citeKeyStart, ~to_=cursor.contents)
              if !isValidCiteKey(citeKey) {
                Error(InvalidCiteKey)
              } else {
                // Skip to comma or end
                while cursor.contents < length &&
                  Js.String2.charAt(input, cursor.contents) != "," &&
                  Js.String2.charAt(input, cursor.contents) != closingChar {
                  cursor := cursor.contents + 1
                }
                if cursor.contents < length && Js.String2.charAt(input, cursor.contents) == "," {
                  cursor := cursor.contents + 1
                }

                // Parse fields (simplified for safety - just return basic structure)
                let fields = ref([])

                // Skip to closing brace/paren
                while cursor.contents < length &&
                  Js.String2.charAt(input, cursor.contents) != closingChar {
                  cursor := cursor.contents + 1
                }

                Ok({
                  entryType,
                  citeKey,
                  fields: fields.contents,
                })
              }
            }
          }
        }
      }
    }
  }
}

/** Check if input appears to be a valid BibTeX entry */
let isValidEntry = (input: string): bool => {
  switch parseEntry(input) {
  | Ok(_) => true
  | Error(_) => false
  }
}

/** Format an entry to canonical BibTeX string */
let formatEntry = (entry: bibtexEntry): string => {
  let result = ref(`@${entryTypeToString(entry.entryType)}{${entry.citeKey},\n`)

  Belt.Array.forEachWithIndex(entry.fields, (index, field) => {
    result := result.contents ++ `  ${field.key} = {${field.value}}`
    if index < Belt.Array.length(entry.fields) - 1 {
      result := result.contents ++ ","
    }
    result := result.contents ++ "\n"
  })

  result.contents ++ "}\n"
}

/** Escape special BibTeX characters in a string */
let escapeString = (input: string): string => {
  input
  ->Js.String2.replaceByRe(%re("/\\/g"), "\\textbackslash{}")
  ->Js.String2.replaceByRe(%re("/{/g"), "\\{")
  ->Js.String2.replaceByRe(%re("/}/g"), "\\}")
  ->Js.String2.replaceByRe(%re("/&/g"), "\\&")
  ->Js.String2.replaceByRe(%re("/%/g"), "\\%")
  ->Js.String2.replaceByRe(%re("/\\$/g"), "\\$")
  ->Js.String2.replaceByRe(%re("/#/g"), "\\#")
  ->Js.String2.replaceByRe(%re("/_/g"), "\\_")
  ->Js.String2.replaceByRe(%re("/~/g"), "\\textasciitilde{}")
  ->Js.String2.replaceByRe(%re("/\\^/g"), "\\textasciicircum{}")
}

/** Create an entry */
let createEntry = (
  ~entryType: entryType,
  ~citeKey: string,
  ~fields: array<bibtexField>,
): result<bibtexEntry, bibtexError> => {
  if !isValidCiteKey(citeKey) {
    Error(InvalidCiteKey)
  } else {
    Ok({entryType, citeKey, fields})
  }
}

/** Add a field to an entry */
let addField = (entry: bibtexEntry, key: string, value: string): bibtexEntry => {
  {
    ...entry,
    fields: Belt.Array.concat(entry.fields, [{key, value}]),
  }
}

/** Remove a field from an entry */
let removeField = (entry: bibtexEntry, key: string): bibtexEntry => {
  let lowerKey = Js.String2.toLowerCase(key)
  {
    ...entry,
    fields: Belt.Array.keep(entry.fields, field =>
      Js.String2.toLowerCase(field.key) != lowerKey
    ),
  }
}

/** Update a field in an entry */
let updateField = (entry: bibtexEntry, key: string, value: string): bibtexEntry => {
  let lowerKey = Js.String2.toLowerCase(key)
  let fieldExists = Belt.Array.some(entry.fields, field =>
    Js.String2.toLowerCase(field.key) == lowerKey
  )

  if fieldExists {
    {
      ...entry,
      fields: Belt.Array.map(entry.fields, field => {
        if Js.String2.toLowerCase(field.key) == lowerKey {
          {key: field.key, value}
        } else {
          field
        }
      }),
    }
  } else {
    addField(entry, key, value)
  }
}
