// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * SafePassword - Password validation and strength checking that cannot crash.
 *
 * Provides safe functions for validating passwords against policies,
 * calculating password strength, and detecting common password patterns.
 * All operations handle edge cases without throwing exceptions.
 */

/** Error types for password operations */
type passwordError =
  | TooShort
  | TooLong
  | NoUppercase
  | NoLowercase
  | NoDigit
  | NoSpecial
  | CommonPassword
  | RepeatingChars
  | SequentialChars

/** Password strength levels */
type strength =
  | VeryWeak
  | Weak
  | Fair
  | Strong
  | VeryStrong

/** Password policy configuration */
type policy = {
  minLength: int,
  maxLength: int,
  requireUppercase: bool,
  requireLowercase: bool,
  requireDigit: bool,
  requireSpecial: bool,
  maxRepeating: int,
  checkCommon: bool,
}

/** Default password policy */
let defaultPolicy: policy = {
  minLength: 8,
  maxLength: 128,
  requireUppercase: true,
  requireLowercase: true,
  requireDigit: true,
  requireSpecial: false,
  maxRepeating: 3,
  checkCommon: true,
}

/** Strong password policy */
let strongPolicy: policy = {
  minLength: 12,
  maxLength: 128,
  requireUppercase: true,
  requireLowercase: true,
  requireDigit: true,
  requireSpecial: true,
  maxRepeating: 2,
  checkCommon: true,
}

/** Special characters for password validation */
let specialChars = "!@#$%^&*()_+-=[]{}|;':\",./<>?`~"

/** Common passwords to check against */
let commonPasswords = [
  "password",
  "123456",
  "12345678",
  "qwerty",
  "abc123",
  "monkey",
  "1234567",
  "letmein",
  "trustno1",
  "dragon",
  "baseball",
  "iloveyou",
  "master",
  "sunshine",
  "ashley",
  "bailey",
  "shadow",
  "123123",
  "654321",
  "superman",
  "qazwsx",
  "michael",
  "football",
  "password1",
  "password123",
  "admin",
]

/** Check if password contains at least one uppercase letter */
let hasUppercase = (password: string): bool => {
  Js.Re.test_(%re("/[A-Z]/"), password)
}

/** Check if password contains at least one lowercase letter */
let hasLowercase = (password: string): bool => {
  Js.Re.test_(%re("/[a-z]/"), password)
}

/** Check if password contains at least one digit */
let hasDigit = (password: string): bool => {
  Js.Re.test_(%re("/[0-9]/"), password)
}

/** Check if password contains at least one special character */
let hasSpecial = (password: string): bool => {
  let length = Js.String2.length(password)
  let found = ref(false)

  for i in 0 to length - 1 {
    if !found.contents {
      let char = Js.String2.charAt(password, i)
      if Js.String2.indexOf(specialChars, char) >= 0 {
        found := true
      }
    }
  }

  found.contents
}

/** Check for repeating characters exceeding the maximum allowed */
let hasRepeatingChars = (password: string, maxRepeating: int): bool => {
  let length = Js.String2.length(password)
  if length < maxRepeating + 1 {
    false
  } else {
    let found = ref(false)
    let count = ref(1)
    let prev = ref(Js.String2.charAt(password, 0))

    for i in 1 to length - 1 {
      if !found.contents {
        let char = Js.String2.charAt(password, i)
        if char == prev.contents {
          count := count.contents + 1
          if count.contents > maxRepeating {
            found := true
          }
        } else {
          count := 1
          prev := char
        }
      }
    }

    found.contents
  }
}

/** Check for sequential characters (abc, 123, etc.) */
let hasSequentialChars = (password: string, sequenceLength: int): bool => {
  let length = Js.String2.length(password)
  if length < sequenceLength {
    false
  } else {
    let found = ref(false)
    let ascending = ref(1)
    let descending = ref(1)

    for i in 1 to length - 1 {
      if !found.contents {
        let prevCode = Js.String2.charCodeAt(password, i - 1)->Belt.Float.toInt
        let currCode = Js.String2.charCodeAt(password, i)->Belt.Float.toInt

        // Check ascending
        if currCode == prevCode + 1 {
          ascending := ascending.contents + 1
          if ascending.contents >= sequenceLength {
            found := true
          }
        } else {
          ascending := 1
        }

        // Check descending
        if currCode + 1 == prevCode {
          descending := descending.contents + 1
          if descending.contents >= sequenceLength {
            found := true
          }
        } else {
          descending := 1
        }
      }
    }

    found.contents
  }
}

/** Check if password is in the common passwords list (case-insensitive prefix match) */
let isCommon = (password: string): bool => {
  let lowerPassword = Js.String2.toLowerCase(password)
  let found = ref(false)

  Belt.Array.forEach(commonPasswords, common => {
    if !found.contents {
      let commonLen = Js.String2.length(common)
      if Js.String2.length(lowerPassword) >= commonLen {
        let prefix = Js.String2.slice(lowerPassword, ~from=0, ~to_=commonLen)
        if prefix == common {
          found := true
        }
      }
    }
  })

  found.contents
}

/** Validate a password against a policy */
let validate = (password: string, policy: policy): result<unit, passwordError> => {
  let length = Js.String2.length(password)

  if length < policy.minLength {
    Error(TooShort)
  } else if length > policy.maxLength {
    Error(TooLong)
  } else if policy.requireUppercase && !hasUppercase(password) {
    Error(NoUppercase)
  } else if policy.requireLowercase && !hasLowercase(password) {
    Error(NoLowercase)
  } else if policy.requireDigit && !hasDigit(password) {
    Error(NoDigit)
  } else if policy.requireSpecial && !hasSpecial(password) {
    Error(NoSpecial)
  } else if hasRepeatingChars(password, policy.maxRepeating) {
    Error(RepeatingChars)
  } else if policy.checkCommon && isCommon(password) {
    Error(CommonPassword)
  } else {
    Ok()
  }
}

/** Check if a password is valid against the default policy */
let isValid = (password: string): bool => {
  switch validate(password, defaultPolicy) {
  | Ok() => true
  | Error(_) => false
  }
}

/** Check if a password is valid against the strong policy */
let isValidStrong = (password: string): bool => {
  switch validate(password, strongPolicy) {
  | Ok() => true
  | Error(_) => false
  }
}

/** Calculate password strength score (0-4) */
let strengthScore = (password: string): int => {
  let score = ref(0)
  let length = Js.String2.length(password)

  // Length score
  if length >= 8 {
    score := score.contents + 1
  }
  if length >= 12 {
    score := score.contents + 1
  }
  if length >= 16 {
    score := score.contents + 1
  }

  // Character variety
  if hasUppercase(password) {
    score := score.contents + 1
  }
  if hasLowercase(password) {
    score := score.contents + 1
  }
  if hasDigit(password) {
    score := score.contents + 1
  }
  if hasSpecial(password) {
    score := score.contents + 2
  }

  // Penalties
  if hasRepeatingChars(password, 2) {
    score := max(0, score.contents - 1)
  }
  if hasSequentialChars(password, 3) {
    score := max(0, score.contents - 1)
  }
  if isCommon(password) {
    score := max(0, score.contents - 3)
  }

  score.contents
}

/** Calculate password strength */
let strength = (password: string): strength => {
  let score = strengthScore(password)

  if score <= 2 {
    VeryWeak
  } else if score <= 4 {
    Weak
  } else if score <= 6 {
    Fair
  } else if score <= 8 {
    Strong
  } else {
    VeryStrong
  }
}

/** Convert strength to a numeric score (0-4) */
let strengthToScore = (s: strength): int => {
  switch s {
  | VeryWeak => 0
  | Weak => 1
  | Fair => 2
  | Strong => 3
  | VeryStrong => 4
  }
}

/** Convert strength to string */
let strengthToString = (s: strength): string => {
  switch s {
  | VeryWeak => "Very Weak"
  | Weak => "Weak"
  | Fair => "Fair"
  | Strong => "Strong"
  | VeryStrong => "Very Strong"
  }
}

/** Calculate password entropy in bits
 *
 * Entropy is calculated based on the character classes used and password length.
 */
let entropy = (password: string): float => {
  let length = Js.String2.length(password)
  if length == 0 {
    0.0
  } else {
    let charsetSize = ref(0.0)

    if hasLowercase(password) {
      charsetSize := charsetSize.contents +. 26.0
    }
    if hasUppercase(password) {
      charsetSize := charsetSize.contents +. 26.0
    }
    if hasDigit(password) {
      charsetSize := charsetSize.contents +. 10.0
    }
    if hasSpecial(password) {
      charsetSize := charsetSize.contents +. 32.0
    }

    if charsetSize.contents == 0.0 {
      0.0
    } else {
      let len = Belt.Int.toFloat(length)
      len *. (Js.Math.log(charsetSize.contents) /. Js.Math.log(2.0))
    }
  }
}

/** Get a description of why password validation failed */
let errorToString = (err: passwordError): string => {
  switch err {
  | TooShort => "Password is too short"
  | TooLong => "Password is too long"
  | NoUppercase => "Password must contain at least one uppercase letter"
  | NoLowercase => "Password must contain at least one lowercase letter"
  | NoDigit => "Password must contain at least one digit"
  | NoSpecial => "Password must contain at least one special character"
  | CommonPassword => "Password is too common"
  | RepeatingChars => "Password contains too many repeating characters"
  | SequentialChars => "Password contains sequential characters"
  }
}

/** Get all validation errors for a password */
let validateAll = (password: string, policy: policy): array<passwordError> => {
  let errors = ref([])
  let length = Js.String2.length(password)

  if length < policy.minLength {
    errors := Belt.Array.concat(errors.contents, [TooShort])
  }
  if length > policy.maxLength {
    errors := Belt.Array.concat(errors.contents, [TooLong])
  }
  if policy.requireUppercase && !hasUppercase(password) {
    errors := Belt.Array.concat(errors.contents, [NoUppercase])
  }
  if policy.requireLowercase && !hasLowercase(password) {
    errors := Belt.Array.concat(errors.contents, [NoLowercase])
  }
  if policy.requireDigit && !hasDigit(password) {
    errors := Belt.Array.concat(errors.contents, [NoDigit])
  }
  if policy.requireSpecial && !hasSpecial(password) {
    errors := Belt.Array.concat(errors.contents, [NoSpecial])
  }
  if hasRepeatingChars(password, policy.maxRepeating) {
    errors := Belt.Array.concat(errors.contents, [RepeatingChars])
  }
  if policy.checkCommon && isCommon(password) {
    errors := Belt.Array.concat(errors.contents, [CommonPassword])
  }

  errors.contents
}

/** Create a custom policy */
let makePolicy = (
  ~minLength: int=8,
  ~maxLength: int=128,
  ~requireUppercase: bool=true,
  ~requireLowercase: bool=true,
  ~requireDigit: bool=true,
  ~requireSpecial: bool=false,
  ~maxRepeating: int=3,
  ~checkCommon: bool=true,
  (),
): policy => {
  {
    minLength: minLength,
    maxLength: maxLength,
    requireUppercase: requireUppercase,
    requireLowercase: requireLowercase,
    requireDigit: requireDigit,
    requireSpecial: requireSpecial,
    maxRepeating: maxRepeating,
    checkCommon: checkCommon,
  }
}
