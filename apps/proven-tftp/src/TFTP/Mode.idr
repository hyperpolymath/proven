-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- TFTP Transfer Modes (RFC 1350 Section 2)
--
-- TFTP supports three transfer modes that control how data is interpreted
-- during transfer. NetASCII and Octet are the commonly used modes; Mail
-- is obsolete but included for protocol completeness. The mode string
-- in request packets is case-insensitive per the RFC.

module TFTP.Mode

%default total

-- ============================================================================
-- TFTP Transfer Modes (RFC 1350 Section 2)
-- ============================================================================

||| The three TFTP transfer modes as defined in RFC 1350.
public export
data TransferMode : Type where
  ||| NetASCII: 8-bit ASCII with CR/LF line endings.
  ||| Used for text file transfers. The sender converts local line
  ||| endings to CR/LF; the receiver converts CR/LF to local format.
  ||| All printable ASCII characters plus CR, LF, and NUL are valid.
  NetASCII : TransferMode
  ||| Octet (binary): raw 8-bit bytes, no translation.
  ||| Used for binary file transfers. Data is transferred verbatim
  ||| with no character or line-ending conversion.
  Octet    : TransferMode
  ||| Mail: used to send files as email (obsolete).
  ||| The destination is a username rather than a filename.
  ||| This mode is effectively deprecated and rarely supported.
  Mail     : TransferMode

public export
Eq TransferMode where
  NetASCII == NetASCII = True
  Octet    == Octet    = True
  Mail     == Mail     = True
  _        == _        = False

public export
Show TransferMode where
  show NetASCII = "netascii"
  show Octet    = "octet"
  show Mail     = "mail"

-- ============================================================================
-- String conversion
-- ============================================================================

||| Convert a transfer mode to its wire-format string.
||| The string is lowercase per convention, though the RFC specifies
||| case-insensitive matching.
public export
modeToString : TransferMode -> String
modeToString NetASCII = "netascii"
modeToString Octet    = "octet"
modeToString Mail     = "mail"

||| Parse a mode string to a TransferMode.
||| Matching is case-insensitive as required by RFC 1350 Section 2.
||| Returns Nothing for unrecognised mode strings.
public export
modeFromString : String -> Maybe TransferMode
modeFromString s =
  let lower = toLower s
  in if lower == "netascii" then Just NetASCII
     else if lower == "octet" then Just Octet
     else if lower == "mail" then Just Mail
     else Nothing

-- ============================================================================
-- Mode classification and properties
-- ============================================================================

||| Whether the mode performs character translation (line ending conversion).
||| Only NetASCII performs translation; Octet and Mail transfer raw bytes
||| (Mail is raw to the mail system).
public export
hasTranslation : TransferMode -> Bool
hasTranslation NetASCII = True
hasTranslation Octet    = False
hasTranslation Mail     = False

||| Whether the mode is deprecated/obsolete.
||| Mail mode is effectively obsolete and rarely supported.
public export
isObsolete : TransferMode -> Bool
isObsolete Mail = True
isObsolete _    = False

||| Whether the mode is suitable for binary file transfer.
||| Only Octet mode guarantees bit-for-bit accuracy.
public export
isBinaryMode : TransferMode -> Bool
isBinaryMode Octet = True
isBinaryMode _     = False

||| Whether the mode is suitable for text file transfer.
||| NetASCII is designed for text with proper line-ending conversion.
public export
isTextMode : TransferMode -> Bool
isTextMode NetASCII = True
isTextMode _        = False

||| Human-readable description of a transfer mode.
public export
modeDescription : TransferMode -> String
modeDescription NetASCII = "8-bit ASCII with CR/LF line endings"
modeDescription Octet    = "Raw 8-bit binary (no translation)"
modeDescription Mail     = "Mail delivery (obsolete)"

-- ============================================================================
-- Line ending conversion for NetASCII
-- ============================================================================

||| The NetASCII line ending: Carriage Return + Line Feed.
||| All line endings must be converted to CR/LF for NetASCII mode.
public export
netasciiLineEnding : List Bits8
netasciiLineEnding = [0x0D, 0x0A]  -- CR, LF

||| Check if a byte sequence contains a bare CR (CR not followed by LF).
||| In NetASCII, bare CR must be represented as CR/NUL.
public export
hasBareCarriageReturn : List Bits8 -> Bool
hasBareCarriageReturn [] = False
hasBareCarriageReturn [0x0D] = True  -- CR at end of data = bare CR
hasBareCarriageReturn (0x0D :: 0x0A :: rest) = hasBareCarriageReturn rest  -- CR/LF is ok
hasBareCarriageReturn (0x0D :: _ :: rest) = True  -- CR followed by non-LF = bare CR
hasBareCarriageReturn (_ :: rest) = hasBareCarriageReturn rest
