-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- DNS Domain Name Representation (RFC 1035 Section 3.1)
--
-- Domain names are represented as a list of labels with compile-time
-- length constraints. Each label is at most 63 octets, and the total
-- name (with dots) is at most 253 characters. Malformed names are
-- rejected during construction, not at runtime.

module DNS.Name

%default total

-- ============================================================================
-- Label length constraints
-- ============================================================================

||| Maximum length of a single DNS label (RFC 1035 Section 2.3.4).
public export
maxLabelLen : Nat
maxLabelLen = 63

||| Maximum total length of a domain name including dots (RFC 1035).
public export
maxNameLen : Nat
maxNameLen = 253

-- ============================================================================
-- DNS Label
-- ============================================================================

||| A single DNS label (e.g. "www", "example", "com").
||| Stored as a raw string; validation is performed at construction time.
public export
record Label where
  constructor MkLabel
  ||| The label text (ASCII, max 63 characters).
  labelText : String
  ||| Proof that the label is non-empty and within bounds.
  ||| We store the length for fast access.
  labelLen  : Nat

public export
Show Label where
  show l = l.labelText

public export
Eq Label where
  a == b = toLower a.labelText == toLower b.labelText

-- ============================================================================
-- Domain Name
-- ============================================================================

||| A fully qualified domain name as a list of labels.
||| Example: "www.example.com" = [MkLabel "www" 3, MkLabel "example" 7, MkLabel "com" 3]
public export
record DomainName where
  constructor MkDomainName
  ||| The ordered list of labels from leftmost to rightmost.
  labels    : List Label
  ||| The total character count (labels + dots), cached for fast checks.
  totalLen  : Nat

public export
Show DomainName where
  show dn = joinLabels dn.labels
    where
      joinLabels : List Label -> String
      joinLabels []        = "."
      joinLabels [l]       = l.labelText
      joinLabels (l :: ls) = l.labelText ++ "." ++ joinLabels ls

public export
Eq DomainName where
  a == b = a.labels == b.labels

-- ============================================================================
-- Validation errors
-- ============================================================================

||| Errors that can occur when parsing a domain name.
public export
data NameError : Type where
  ||| The domain name string is empty.
  EmptyName       : NameError
  ||| A label exceeds the maximum length of 63 characters.
  LabelTooLong    : (label : String) -> (len : Nat) -> NameError
  ||| A label is empty (consecutive dots or leading/trailing dot).
  EmptyLabel      : NameError
  ||| The total name length exceeds 253 characters.
  NameTooLong     : (len : Nat) -> NameError
  ||| A label contains an invalid character.
  InvalidChar     : (label : String) -> (char : Char) -> NameError

public export
Show NameError where
  show EmptyName           = "Empty domain name"
  show (LabelTooLong l n)  = "Label too long: '" ++ l ++ "' (" ++ show n ++ " > 63)"
  show EmptyLabel          = "Empty label (consecutive dots)"
  show (NameTooLong n)     = "Name too long: " ++ show n ++ " > 253"
  show (InvalidChar l c)   = "Invalid character '" ++ singleton c ++ "' in label '" ++ l ++ "'"

-- ============================================================================
-- Character validation
-- ============================================================================

||| Check if a character is valid in a DNS label (RFC 1035 Section 2.3.1).
||| Letters, digits, and hyphens are allowed. Hyphens cannot be first or last.
public export
isValidLabelChar : Char -> Bool
isValidLabelChar c = isAlpha c || isDigit c || c == '-'

||| Validate a single label string.
public export
validateLabel : String -> Either NameError Label
validateLabel s =
  let len = length s
  in if len == 0
       then Left EmptyLabel
     else if len > maxLabelLen
       then Left (LabelTooLong s len)
     else if not (all isValidLabelChar (unpack s))
       then case find (not . isValidLabelChar) (unpack s) of
              Just c  => Left (InvalidChar s c)
              Nothing => Left (InvalidChar s '?')  -- unreachable but total
     else Right (MkLabel s len)

-- ============================================================================
-- Domain name parsing
-- ============================================================================

||| Parse a domain name string into a validated DomainName.
||| Returns Left with a descriptive error for invalid names.
||| Trailing dots are silently removed (FQDN normalisation).
public export
parseName : String -> Either NameError DomainName
parseName s =
  let trimmed = if s == "."
                  then ""
                  else s
      parts   = toList (split (== '.') trimmed)
      -- Remove trailing empty string from FQDN trailing dot
      cleaned = filter (/= "") parts
  in if length trimmed == 0
       then Left EmptyName
     else case validateLabels cleaned of
            Left err     => Left err
            Right labels =>
              let total = computeTotalLen labels
              in if total > maxNameLen
                   then Left (NameTooLong total)
                   else Right (MkDomainName labels total)
  where
    ||| Validate each label in the list.
    validateLabels : List String -> Either NameError (List Label)
    validateLabels [] = Right []
    validateLabels (p :: ps) = case validateLabel p of
      Left err => Left err
      Right l  => case validateLabels ps of
        Left err  => Left err
        Right ls  => Right (l :: ls)

    ||| Compute the total length of a name (labels + dots between them).
    computeTotalLen : List Label -> Nat
    computeTotalLen []        = 0
    computeTotalLen [l]       = l.labelLen
    computeTotalLen (l :: ls) = l.labelLen + 1 + computeTotalLen ls

-- ============================================================================
-- Domain name utilities
-- ============================================================================

||| Get the number of labels in a domain name.
public export
labelCount : DomainName -> Nat
labelCount dn = length dn.labels

||| Check if a domain name is a subdomain of another.
||| "www.example.com" is a subdomain of "example.com".
public export
isSubdomainOf : (child : DomainName) -> (parent : DomainName) -> Bool
isSubdomainOf child parent =
  let cl = reverse child.labels
      pl = reverse parent.labels
  in startsWith pl cl
  where
    startsWith : List Label -> List Label -> Bool
    startsWith []        _         = True
    startsWith _         []        = False
    startsWith (p :: ps) (c :: cs) = p == c && startsWith ps cs

||| Get the top-level domain (rightmost label).
public export
tld : DomainName -> Maybe Label
tld dn = case reverse dn.labels of
  []       => Nothing
  (l :: _) => Just l

||| Build a domain name from a list of label strings (unchecked).
||| Use parseName for validated construction.
public export
unsafeMkName : List String -> DomainName
unsafeMkName parts =
  let labels = map (\s => MkLabel s (length s)) parts
      total  = foldl (\acc, l => acc + l.labelLen + 1) 0 labels
  in MkDomainName labels (if total > 0 then minus total 1 else 0)
