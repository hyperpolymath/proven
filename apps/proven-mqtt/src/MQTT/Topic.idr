-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- MQTT Topic Names and Topic Filters (MQTT 3.1.1 Section 4.7)
--
-- Topic names are hierarchical strings separated by '/'. Topic filters
-- allow single-level (+) and multi-level (#) wildcards for subscriptions.
-- This module validates topics at the type level: null characters are
-- rejected, empty segments are rejected, and wildcard placement rules
-- are enforced — all before any network I/O occurs.

module MQTT.Topic

%default total

-- ============================================================================
-- Topic validation results
-- ============================================================================

||| Errors that can occur when validating a topic string.
public export
data TopicError : Type where
  ||| Topic string is empty (MQTT 3.1.1 Section 4.7.3: must be >= 1 char).
  EmptyTopic         : TopicError
  ||| Topic contains a null character (U+0000), which is forbidden.
  NullCharPresent    : TopicError
  ||| Topic exceeds the maximum length of 65535 bytes (MQTT 3.1.1 Section 4.7.3).
  TopicTooLong       : (actual : Nat) -> TopicError
  ||| An empty segment was found (e.g., "a//b"). Only allowed in topic names,
  ||| not in topic filters with wildcards.
  EmptySegment       : TopicError
  ||| The '#' wildcard must be the last character and must be preceded by '/'.
  InvalidMultiLevel  : TopicError
  ||| The '+' wildcard must occupy an entire segment by itself.
  InvalidSingleLevel : TopicError

public export
Show TopicError where
  show EmptyTopic           = "Topic must not be empty"
  show NullCharPresent      = "Topic must not contain null characters"
  show (TopicTooLong n)     = "Topic too long: " ++ show n ++ " bytes (max 65535)"
  show EmptySegment         = "Topic contains empty segment"
  show InvalidMultiLevel    = "'#' wildcard must be last and preceded by '/'"
  show InvalidSingleLevel   = "'+' wildcard must occupy entire segment"

-- ============================================================================
-- Topic segments
-- ============================================================================

||| A single segment of a topic hierarchy.
||| Segments are separated by '/' in the full topic string.
public export
data TopicSegment : Type where
  ||| A literal segment containing no wildcards.
  Literal     : (content : String) -> TopicSegment
  ||| The single-level wildcard '+', matching exactly one level.
  SingleLevel : TopicSegment
  ||| The multi-level wildcard '#', matching zero or more levels.
  MultiLevel  : TopicSegment

public export
Eq TopicSegment where
  (Literal a)  == (Literal b)  = a == b
  SingleLevel  == SingleLevel  = True
  MultiLevel   == MultiLevel   = True
  _            == _            = False

public export
Show TopicSegment where
  show (Literal s)  = s
  show SingleLevel  = "+"
  show MultiLevel   = "#"

-- ============================================================================
-- Validated topic types
-- ============================================================================

||| A validated MQTT topic name (no wildcards allowed).
||| Used in PUBLISH packets for the destination topic.
public export
record TopicName where
  constructor MkTopicName
  ||| The raw topic string, validated to contain no wildcards or null chars.
  raw      : String
  ||| The parsed segments of the topic hierarchy.
  segments : List TopicSegment

public export
Show TopicName where
  show tn = tn.raw

public export
Eq TopicName where
  a == b = a.raw == b.raw

||| A validated MQTT topic filter (wildcards allowed).
||| Used in SUBSCRIBE/UNSUBSCRIBE packets.
public export
record TopicFilter where
  constructor MkTopicFilter
  ||| The raw filter string, validated for correct wildcard placement.
  raw      : String
  ||| The parsed segments including wildcard markers.
  segments : List TopicSegment

public export
Show TopicFilter where
  show tf = tf.raw

public export
Eq TopicFilter where
  a == b = a.raw == b.raw

-- ============================================================================
-- Validation helpers
-- ============================================================================

||| Check if a string contains a null character (U+0000).
public export
containsNull : String -> Bool
containsNull s = any (== '\0') (unpack s)

||| Split a topic string into segments by '/'.
public export
splitTopic : String -> List String
splitTopic s = map pack (splitOn '/' (unpack s))
  where
    splitOn : Char -> List Char -> List (List Char)
    splitOn _   [] = [[]]
    splitOn sep (c :: cs) =
      if c == sep
        then [] :: splitOn sep cs
        else case splitOn sep cs of
               []        => [[c]]
               (s :: ss) => (c :: s) :: ss

||| Classify a raw segment string as a TopicSegment.
public export
classifySegment : String -> TopicSegment
classifySegment "+" = SingleLevel
classifySegment "#" = MultiLevel
classifySegment s   = Literal s

||| Check whether a segment list has a valid multi-level wildcard placement.
||| '#' must be the last segment if present at all.
public export
validMultiLevel : List TopicSegment -> Bool
validMultiLevel [] = True
validMultiLevel [MultiLevel] = True
validMultiLevel (MultiLevel :: _) = False
validMultiLevel (_ :: rest) = validMultiLevel rest

||| Check whether single-level wildcards are validly placed.
||| '+' must occupy an entire segment (already guaranteed by classifySegment),
||| but we also verify no segment contains '+' mixed with other characters.
public export
validSingleLevel : List String -> Bool
validSingleLevel [] = True
validSingleLevel (s :: rest) =
  let chars = unpack s
      hasMixed = any (== '+') chars && length chars > 1
  in not hasMixed && validSingleLevel rest

-- ============================================================================
-- Topic name validation (no wildcards)
-- ============================================================================

||| Validate and construct a TopicName from a raw string.
||| Rejects empty strings, null characters, topics exceeding 65535 bytes,
||| and any wildcard characters.
public export
mkTopicName : String -> Either TopicError TopicName
mkTopicName s =
  if s == "" then Left EmptyTopic
  else if containsNull s then Left NullCharPresent
  else if length s > 65535 then Left (TopicTooLong (length s))
  else
    let rawSegments = splitTopic s
        segments = map classifySegment rawSegments
        hasWildcard = any (\seg => case seg of
                                     SingleLevel => True
                                     MultiLevel  => True
                                     Literal _   => False) segments
    in if hasWildcard
         then Left InvalidSingleLevel  -- wildcards not allowed in topic names
         else Right (MkTopicName s segments)

-- ============================================================================
-- Topic filter validation (wildcards allowed with rules)
-- ============================================================================

||| Validate and construct a TopicFilter from a raw string.
||| Rejects empty strings, null characters, topics exceeding 65535 bytes,
||| mis-placed '#' wildcards, and '+' mixed with other characters in a segment.
public export
mkTopicFilter : String -> Either TopicError TopicFilter
mkTopicFilter s =
  if s == "" then Left EmptyTopic
  else if containsNull s then Left NullCharPresent
  else if length s > 65535 then Left (TopicTooLong (length s))
  else
    let rawSegments = splitTopic s
        segments = map classifySegment rawSegments
    in if not (validSingleLevel rawSegments)
         then Left InvalidSingleLevel
       else if not (validMultiLevel segments)
         then Left InvalidMultiLevel
       else Right (MkTopicFilter s segments)

-- ============================================================================
-- Topic matching (MQTT 3.1.1 Section 4.7.1-4.7.2)
-- ============================================================================

||| Match a topic name against a topic filter.
||| Returns True if the topic name matches the filter pattern.
||| Implements MQTT 3.1.1 Section 4.7 matching rules:
|||   - Literal segments must match exactly
|||   - '+' matches exactly one level
|||   - '#' matches zero or more trailing levels
public export
topicMatches : TopicName -> TopicFilter -> Bool
topicMatches name filter = matchSegments name.segments filter.segments
  where
    matchSegments : List TopicSegment -> List TopicSegment -> Bool
    matchSegments []        []              = True
    matchSegments _         [MultiLevel]    = True
    matchSegments []        _               = False
    matchSegments (_ :: ns) (SingleLevel :: fs) = matchSegments ns fs
    matchSegments ((Literal a) :: ns) ((Literal b) :: fs) =
      a == b && matchSegments ns fs
    matchSegments _ _ = False
