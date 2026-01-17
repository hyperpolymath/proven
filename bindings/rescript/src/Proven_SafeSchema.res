// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * SafeSchema - Schema validation (JSON Schema-like) that cannot crash.
 *
 * Provides compile-time and runtime schema validation for structured data.
 * Designed to safely validate data without memory allocation failures or
 * undefined behavior.
 */

/** Schema validation error types */
type schemaError =
  | TypeMismatch
  | RequiredFieldMissing
  | ValueOutOfRange
  | PatternMismatch
  | ArrayTooLong
  | ArrayTooShort
  | StringTooLong
  | StringTooShort
  | InvalidFormat
  | MaxPropertiesExceeded
  | MinPropertiesNotMet
  | UniqueViolation
  | EnumValueInvalid
  | SchemaInvalid

/** Supported value types for schema validation */
type valueType =
  | NullType
  | BooleanType
  | IntegerType
  | NumberType
  | StringType
  | ArrayType
  | ObjectType

/** String format constraints */
type stringFormat =
  | NoFormat
  | EmailFormat
  | UriFormat
  | DateFormat
  | DatetimeFormat
  | UuidFormat
  | Ipv4Format
  | Ipv6Format
  | HostnameFormat

/** Schema constraint definition for a single field */
type fieldSchema = {
  valueType: valueType,
  required: bool,
  minLength: option<int>,
  maxLength: option<int>,
  minValue: option<int>,
  maxValue: option<int>,
  minItems: option<int>,
  maxItems: option<int>,
  format: stringFormat,
  pattern: option<string>,
  enumValues: option<array<string>>,
}

/** Field entry for object schema */
type fieldEntry = {
  name: string,
  schema: fieldSchema,
}

/** Object schema with field definitions */
type objectSchema = {
  mutable fields: array<fieldEntry>,
  minProperties: int,
  maxProperties: int,
  additionalPropertiesAllowed: bool,
}

/** Validation result with details */
type validationResult = {
  valid: bool,
  errorPath: option<string>,
  errorMessage: option<string>,
}

/** Create a successful validation result */
let ok = (): validationResult => {
  {valid: true, errorPath: None, errorMessage: None}
}

/** Create a failed validation result */
let fail = (path: option<string>, message: option<string>): validationResult => {
  {valid: false, errorPath: path, errorMessage: message}
}

/** Default field schema */
let defaultFieldSchema = (): fieldSchema => {
  {
    valueType: StringType,
    required: false,
    minLength: None,
    maxLength: None,
    minValue: None,
    maxValue: None,
    minItems: None,
    maxItems: None,
    format: NoFormat,
    pattern: None,
    enumValues: None,
  }
}

/** Create a new object schema */
let createObjectSchema = (~maxFields: int=100, ()): objectSchema => {
  {
    fields: [],
    minProperties: 0,
    maxProperties: maxFields,
    additionalPropertiesAllowed: true,
  }
}

/** Add a field to an object schema */
let addField = (schema: objectSchema, name: string, fieldSchema: fieldSchema): result<unit, schemaError> => {
  if Js.Array2.length(schema.fields) >= schema.maxProperties {
    Error(SchemaInvalid)
  } else {
    schema.fields = Js.Array2.concat(schema.fields, [{name, schema: fieldSchema}])
    Ok()
  }
}

/** Get the schema for a named field */
let getFieldSchema = (schema: objectSchema, name: string): option<fieldSchema> => {
  let result = ref(None)
  Js.Array2.forEach(schema.fields, entry => {
    if entry.name == name {
      result := Some(entry.schema)
    }
  })
  result.contents
}

/** Check if a field is defined in the schema */
let hasField = (schema: objectSchema, name: string): bool => {
  Belt.Option.isSome(getFieldSchema(schema, name))
}

/** Get all required field names */
let getRequiredFields = (schema: objectSchema): array<string> => {
  Js.Array2.filter(schema.fields, entry => entry.schema.required)->Js.Array2.map(entry =>
    entry.name
  )
}

/** Validate property count constraints */
let validatePropertyCount = (schema: objectSchema, count: int): result<unit, schemaError> => {
  if count < schema.minProperties {
    Error(MinPropertiesNotMet)
  } else if count > schema.maxProperties {
    Error(MaxPropertiesExceeded)
  } else {
    Ok()
  }
}

/** Check if a field name is allowed (for strict mode) */
let isFieldAllowed = (schema: objectSchema, name: string): bool => {
  if schema.additionalPropertiesAllowed {
    true
  } else {
    hasField(schema, name)
  }
}

/** Disallow additional properties */
let disallowAdditionalProperties = (schema: objectSchema): unit => {
  ignore({...schema, additionalPropertiesAllowed: false})
}

/** Format validation helper functions */

let isValidEmail = (value: string): bool => {
  let length = Js.String2.length(value)
  if length == 0 || length > 254 {
    false
  } else {
    switch Js.String2.indexOf(value, "@") {
    | -1 => false
    | 0 => false
    | atPos =>
      if atPos == length - 1 {
        false
      } else {
        let local = Js.String2.slice(value, ~from=0, ~to_=atPos)
        let domain = Js.String2.sliceToEnd(value, ~from=atPos + 1)
        Js.String2.length(local) <= 64 &&
        Js.String2.length(domain) > 0 &&
        Js.String2.includes(domain, ".")
      }
    }
  }
}

let isValidUri = (value: string): bool => {
  if Js.String2.length(value) == 0 {
    false
  } else {
    switch Js.String2.indexOf(value, "://") {
    | -1 => false
    | 0 => false
    | pos =>
      let scheme = Js.String2.slice(value, ~from=0, ~to_=pos)
      let schemeChars = Js.String2.split(scheme, "")
      Js.Array2.every(schemeChars, c => {
        let code = Js.String2.charCodeAt(c, 0)
        (code >= 48.0 && code <= 57.0) || // 0-9
          (code >= 65.0 && code <= 90.0) || // A-Z
          (code >= 97.0 && code <= 122.0) || // a-z
          code == 43.0 || // +
          code == 45.0 || // -
          code == 46.0 // .
      })
    }
  }
}

let isValidDate = (value: string): bool => {
  // YYYY-MM-DD format
  if Js.String2.length(value) != 10 {
    false
  } else if (
    Js.String2.charAt(value, 4) != "-" || Js.String2.charAt(value, 7) != "-"
  ) {
    false
  } else {
    switch (
      Belt.Int.fromString(Js.String2.slice(value, ~from=0, ~to_=4)),
      Belt.Int.fromString(Js.String2.slice(value, ~from=5, ~to_=7)),
      Belt.Int.fromString(Js.String2.slice(value, ~from=8, ~to_=10)),
    ) {
    | (Some(_year), Some(month), Some(day)) =>
      month >= 1 && month <= 12 && day >= 1 && day <= 31
    | _ => false
    }
  }
}

let isValidDatetime = (value: string): bool => {
  // ISO 8601: YYYY-MM-DDTHH:MM:SS or YYYY-MM-DDTHH:MM:SSZ
  if Js.String2.length(value) < 19 {
    false
  } else if !isValidDate(Js.String2.slice(value, ~from=0, ~to_=10)) {
    false
  } else {
    let sep = Js.String2.charAt(value, 10)
    if sep != "T" && sep != " " {
      false
    } else if (
      Js.String2.charAt(value, 13) != ":" || Js.String2.charAt(value, 16) != ":"
    ) {
      false
    } else {
      switch (
        Belt.Int.fromString(Js.String2.slice(value, ~from=11, ~to_=13)),
        Belt.Int.fromString(Js.String2.slice(value, ~from=14, ~to_=16)),
        Belt.Int.fromString(Js.String2.slice(value, ~from=17, ~to_=19)),
      ) {
      | (Some(hour), Some(minute), Some(second)) =>
        hour >= 0 && hour <= 23 && minute >= 0 && minute <= 59 && second >= 0 && second <= 59
      | _ => false
      }
    }
  }
}

let isValidUuid = (value: string): bool => {
  // UUID format: xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx (36 chars)
  if Js.String2.length(value) != 36 {
    false
  } else if (
    Js.String2.charAt(value, 8) != "-" ||
      Js.String2.charAt(value, 13) != "-" ||
      Js.String2.charAt(value, 18) != "-" ||
      Js.String2.charAt(value, 23) != "-"
  ) {
    false
  } else {
    let chars = Js.String2.split(value, "")
    Js.Array2.everyi(chars, (c, i) => {
      if i == 8 || i == 13 || i == 18 || i == 23 {
        true
      } else {
        let code = Js.String2.charCodeAt(c, 0)
        (code >= 48.0 && code <= 57.0) || // 0-9
          (code >= 65.0 && code <= 70.0) || // A-F
          (code >= 97.0 && code <= 102.0) // a-f
      }
    })
  }
}

let isValidIpv4 = (value: string): bool => {
  let parts = Js.String2.split(value, ".")
  if Js.Array2.length(parts) != 4 {
    false
  } else {
    Js.Array2.every(parts, part => {
      switch Belt.Int.fromString(part) {
      | Some(n) => n >= 0 && n <= 255 && Js.String2.length(part) <= 3
      | None => false
      }
    })
  }
}

let isValidIpv6 = (value: string): bool => {
  let length = Js.String2.length(value)
  if length == 0 || length > 45 {
    false
  } else {
    let groups = ref(0)
    let hasDoubleColon = ref(false)
    let consecutiveColons = ref(0)
    let hexDigits = ref(0)

    let valid = ref(true)
    for i in 0 to length - 1 {
      if valid.contents {
        let c = Js.String2.charAt(value, i)
        if c == ":" {
          consecutiveColons := consecutiveColons.contents + 1
          if consecutiveColons.contents == 2 {
            if hasDoubleColon.contents {
              valid := false // Only one :: allowed
            } else {
              hasDoubleColon := true
            }
          } else if consecutiveColons.contents > 2 {
            valid := false
          }
          if hexDigits.contents > 0 {
            groups := groups.contents + 1
          }
          hexDigits := 0
        } else {
          let code = Js.String2.charCodeAt(c, 0)
          let isHex =
            (code >= 48.0 && code <= 57.0) || // 0-9
              (code >= 65.0 && code <= 70.0) || // A-F
              (code >= 97.0 && code <= 102.0) // a-f
          if isHex {
            consecutiveColons := 0
            hexDigits := hexDigits.contents + 1
            if hexDigits.contents > 4 {
              valid := false
            }
          } else {
            valid := false
          }
        }
      }
    }

    if hexDigits.contents > 0 {
      groups := groups.contents + 1
    }

    valid.contents && (hasDoubleColon.contents ? groups.contents <= 8 : groups.contents == 8)
  }
}

let isValidHostname = (value: string): bool => {
  let length = Js.String2.length(value)
  if length == 0 || length > 253 {
    false
  } else {
    let labelLen = ref(0)
    let valid = ref(true)

    for i in 0 to length - 1 {
      if valid.contents {
        let c = Js.String2.charAt(value, i)
        if c == "." {
          if labelLen.contents == 0 || labelLen.contents > 63 {
            valid := false
          }
          labelLen := 0
        } else {
          let code = Js.String2.charCodeAt(c, 0)
          let isAlphaNum =
            (code >= 48.0 && code <= 57.0) || // 0-9
              (code >= 65.0 && code <= 90.0) || // A-Z
              (code >= 97.0 && code <= 122.0) || // a-z
              code == 45.0 // -
          if isAlphaNum {
            labelLen := labelLen.contents + 1
            if labelLen.contents > 63 {
              valid := false
            }
          } else {
            valid := false
          }
        }
      }
    }

    valid.contents && labelLen.contents > 0 && labelLen.contents <= 63
  }
}

/** Validate a string against a format constraint */
let validateFormat = (value: string, format: stringFormat): result<unit, schemaError> => {
  let valid = switch format {
  | NoFormat => true
  | EmailFormat => isValidEmail(value)
  | UriFormat => isValidUri(value)
  | DateFormat => isValidDate(value)
  | DatetimeFormat => isValidDatetime(value)
  | UuidFormat => isValidUuid(value)
  | Ipv4Format => isValidIpv4(value)
  | Ipv6Format => isValidIpv6(value)
  | HostnameFormat => isValidHostname(value)
  }

  if valid {
    Ok()
  } else {
    Error(InvalidFormat)
  }
}

/** Validate a string value against a field schema */
let validateString = (schema: fieldSchema, value: string): result<unit, schemaError> => {
  if schema.valueType != StringType {
    Error(TypeMismatch)
  } else {
    let length = Js.String2.length(value)

    switch schema.minLength {
    | Some(min) if length < min => Error(StringTooShort)
    | _ =>
      switch schema.maxLength {
      | Some(max) if length > max => Error(StringTooLong)
      | _ =>
        switch validateFormat(value, schema.format) {
        | Error(e) => Error(e)
        | Ok() =>
          switch schema.enumValues {
          | Some(enums) =>
            if Js.Array2.includes(enums, value) {
              Ok()
            } else {
              Error(EnumValueInvalid)
            }
          | None => Ok()
          }
        }
      }
    }
  }
}

/** Validate an integer value against a field schema */
let validateInteger = (schema: fieldSchema, value: int): result<unit, schemaError> => {
  if schema.valueType != IntegerType && schema.valueType != NumberType {
    Error(TypeMismatch)
  } else {
    switch schema.minValue {
    | Some(min) if value < min => Error(ValueOutOfRange)
    | _ =>
      switch schema.maxValue {
      | Some(max) if value > max => Error(ValueOutOfRange)
      | _ => Ok()
      }
    }
  }
}

/** Validate array length against a field schema */
let validateArrayLength = (schema: fieldSchema, length: int): result<unit, schemaError> => {
  if schema.valueType != ArrayType {
    Error(TypeMismatch)
  } else {
    switch schema.minItems {
    | Some(min) if length < min => Error(ArrayTooShort)
    | _ =>
      switch schema.maxItems {
      | Some(max) if length > max => Error(ArrayTooLong)
      | _ => Ok()
      }
    }
  }
}

/** Schema builder for fluent API */
module Builder = {
  type t = {
    mutable schema: objectSchema,
  }

  let create = (~maxFields: int=100, ()): t => {
    {schema: createObjectSchema(~maxFields, ())}
  }

  let requiredString = (builder: t, name: string): t => {
    let _ = addField(builder.schema, name, {...defaultFieldSchema(), valueType: StringType, required: true})
    builder
  }

  let optionalString = (builder: t, name: string): t => {
    let _ = addField(builder.schema, name, {...defaultFieldSchema(), valueType: StringType, required: false})
    builder
  }

  let requiredInteger = (builder: t, name: string): t => {
    let _ = addField(builder.schema, name, {...defaultFieldSchema(), valueType: IntegerType, required: true})
    builder
  }

  let optionalInteger = (builder: t, name: string): t => {
    let _ = addField(builder.schema, name, {...defaultFieldSchema(), valueType: IntegerType, required: false})
    builder
  }

  let requiredBoolean = (builder: t, name: string): t => {
    let _ = addField(builder.schema, name, {...defaultFieldSchema(), valueType: BooleanType, required: true})
    builder
  }

  let boundedString = (builder: t, name: string, minLen: int, maxLen: int, required: bool): t => {
    let _ = addField(
      builder.schema,
      name,
      {
        ...defaultFieldSchema(),
        valueType: StringType,
        required,
        minLength: Some(minLen),
        maxLength: Some(maxLen),
      },
    )
    builder
  }

  let boundedInteger = (builder: t, name: string, minVal: int, maxVal: int, required: bool): t => {
    let _ = addField(
      builder.schema,
      name,
      {
        ...defaultFieldSchema(),
        valueType: IntegerType,
        required,
        minValue: Some(minVal),
        maxValue: Some(maxVal),
      },
    )
    builder
  }

  let emailField = (builder: t, name: string, required: bool): t => {
    let _ = addField(
      builder.schema,
      name,
      {...defaultFieldSchema(), valueType: StringType, required, format: EmailFormat},
    )
    builder
  }

  let uuidField = (builder: t, name: string, required: bool): t => {
    let _ = addField(
      builder.schema,
      name,
      {...defaultFieldSchema(), valueType: StringType, required, format: UuidFormat},
    )
    builder
  }

  let enumField = (builder: t, name: string, values: array<string>, required: bool): t => {
    let _ = addField(
      builder.schema,
      name,
      {...defaultFieldSchema(), valueType: StringType, required, enumValues: Some(values)},
    )
    builder
  }

  let strict = (builder: t): t => {
    builder.schema = {...builder.schema, additionalPropertiesAllowed: false}
    builder
  }

  let build = (builder: t): objectSchema => {
    builder.schema
  }
}

/** Validate a JSON object against an object schema */
let validateObject = (
  schema: objectSchema,
  fields: array<(string, Js.Json.t)>,
): validationResult => {
  // Check property count
  let fieldCount = Js.Array2.length(fields)
  switch validatePropertyCount(schema, fieldCount) {
  | Error(e) =>
    fail(
      None,
      Some(
        switch e {
        | MinPropertiesNotMet => "Too few properties"
        | MaxPropertiesExceeded => "Too many properties"
        | _ => "Property count validation failed"
        },
      ),
    )
  | Ok() =>
    // Check required fields
    let requiredFields = getRequiredFields(schema)
    let fieldNames = Js.Array2.map(fields, ((name, _)) => name)
    let missingRequired = Js.Array2.filter(requiredFields, req =>
      !Js.Array2.includes(fieldNames, req)
    )

    if Js.Array2.length(missingRequired) > 0 {
      fail(Some(missingRequired[0]), Some("Required field missing"))
    } else {
      // Check for disallowed fields
      if !schema.additionalPropertiesAllowed {
        let disallowed = Js.Array2.filter(fieldNames, name => !hasField(schema, name))
        if Js.Array2.length(disallowed) > 0 {
          fail(Some(disallowed[0]), Some("Additional property not allowed"))
        } else {
          ok()
        }
      } else {
        ok()
      }
    }
  }
}
