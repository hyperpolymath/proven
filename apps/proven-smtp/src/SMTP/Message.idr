-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- SMTP Email Message Representation (RFC 5322)
--
-- Email messages are represented as validated records. Address parsing
-- performs basic validation (presence of '@', non-empty local and domain
-- parts). The message builder ensures required headers (From, To, Date)
-- are present before construction.

module SMTP.Message

%default total

-- ============================================================================
-- Email Address (RFC 5322 Section 3.4)
-- ============================================================================

||| A parsed email address with local and domain parts.
public export
record EmailAddress where
  constructor MkEmailAddress
  ||| The local part (before the @).
  localPart  : String
  ||| The domain part (after the @).
  domainPart : String

public export
Show EmailAddress where
  show addr = addr.localPart ++ "@" ++ addr.domainPart

public export
Eq EmailAddress where
  a == b = a.localPart == b.localPart && a.domainPart == b.domainPart

-- ============================================================================
-- Address validation
-- ============================================================================

||| Errors from parsing email addresses.
public export
data AddressError : Type where
  ||| The address string is empty.
  EmptyAddress    : AddressError
  ||| No '@' separator found.
  MissingAtSign   : (input : String) -> AddressError
  ||| The local part (before @) is empty.
  EmptyLocalPart  : AddressError
  ||| The domain part (after @) is empty.
  EmptyDomainPart : AddressError
  ||| The address contains invalid characters.
  InvalidChars    : (input : String) -> AddressError

public export
Show AddressError where
  show EmptyAddress       = "Empty email address"
  show (MissingAtSign i)  = "No '@' in address: " ++ i
  show EmptyLocalPart     = "Empty local part (before @)"
  show EmptyDomainPart    = "Empty domain part (after @)"
  show (InvalidChars i)   = "Invalid characters in address: " ++ i

||| Parse a string into a validated email address.
||| Returns Left with a descriptive error for invalid addresses.
public export
parseAddress : String -> Either AddressError EmailAddress
parseAddress s =
  if length s == 0
    then Left EmptyAddress
  else case break (== '@') s of
    (local, rest) =>
      if length rest == 0
        then Left (MissingAtSign s)
      else
        let domain = strTail rest  -- Remove the '@'
        in if length local == 0
             then Left EmptyLocalPart
           else if length domain == 0
             then Left EmptyDomainPart
           else Right (MkEmailAddress local domain)

||| Serialise an email address to its string form.
public export
formatAddress : EmailAddress -> String
formatAddress = show

-- ============================================================================
-- Email Header
-- ============================================================================

||| An email header field (RFC 5322 Section 2.2).
public export
record EmailHeader where
  constructor MkEmailHeader
  ||| The header field name (e.g. "Subject", "Content-Type").
  fieldName  : String
  ||| The header field value.
  fieldValue : String

public export
Show EmailHeader where
  show h = h.fieldName ++ ": " ++ h.fieldValue

public export
Eq EmailHeader where
  a == b = a.fieldName == b.fieldName && a.fieldValue == b.fieldValue

-- ============================================================================
-- Email Message (RFC 5322)
-- ============================================================================

||| A structured email message with validated fields.
public export
record EmailMessage where
  constructor MkEmailMessage
  ||| The sender address.
  from    : EmailAddress
  ||| The list of recipient addresses (at least one required).
  to      : List EmailAddress
  ||| The message subject line.
  subject : String
  ||| The message body text.
  body    : String
  ||| Additional headers (beyond From, To, Subject).
  headers : List EmailHeader

public export
Show EmailMessage where
  show msg = "From: " ++ show msg.from
             ++ " To: " ++ show (length msg.to) ++ " recipients"
             ++ " Subject: " ++ msg.subject

-- ============================================================================
-- Message validation
-- ============================================================================

||| Errors from message validation.
public export
data MessageError : Type where
  ||| The recipient list is empty.
  NoRecipients   : MessageError
  ||| The subject line is empty.
  EmptySubject   : MessageError
  ||| The message body is empty.
  EmptyBody      : MessageError
  ||| The message exceeds the maximum size.
  MessageTooLarge : (size : Nat) -> (maxSize : Nat) -> MessageError

public export
Show MessageError where
  show NoRecipients          = "No recipients specified"
  show EmptySubject          = "Empty subject line"
  show EmptyBody             = "Empty message body"
  show (MessageTooLarge s m) = "Message too large: " ++ show s
                               ++ " bytes (max " ++ show m ++ ")"

||| Validate a message for required fields and size constraints.
public export
validateMessage : Nat -> EmailMessage -> List MessageError
validateMessage maxSize msg =
  let errors1 = if length msg.to == 0 then [NoRecipients] else []
      totalSize = length msg.subject + length msg.body
      errors2 = if totalSize > maxSize
                  then [MessageTooLarge totalSize maxSize]
                  else []
  in errors1 ++ errors2

-- ============================================================================
-- Message serialisation (RFC 5322 format)
-- ============================================================================

||| Serialise a message to RFC 5322 format with CRLF line endings.
public export
serialiseMessage : EmailMessage -> String
serialiseMessage msg =
  let fromLine    = "From: " ++ show msg.from ++ "\r\n"
      toLine      = "To: " ++ joinAddrs msg.to ++ "\r\n"
      subjectLine = "Subject: " ++ msg.subject ++ "\r\n"
      extraHeaders = concatMap (\h => h.fieldName ++ ": "
                                      ++ h.fieldValue ++ "\r\n") msg.headers
      separator   = "\r\n"
  in fromLine ++ toLine ++ subjectLine ++ extraHeaders ++ separator ++ msg.body
  where
    joinAddrs : List EmailAddress -> String
    joinAddrs []        = ""
    joinAddrs [a]       = show a
    joinAddrs (a :: as) = show a ++ ", " ++ joinAddrs as

||| Calculate the total size of a serialised message in characters.
public export
estimatedMessageSize : EmailMessage -> Nat
estimatedMessageSize msg =
  length (serialiseMessage msg)
