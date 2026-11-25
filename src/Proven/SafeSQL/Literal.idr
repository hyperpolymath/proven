-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
module Proven.SafeSQL.Literal

import Data.List
import Data.String
import Decidable.Equality

%default total

||| SQL string syntax is a connection setting, not just a database brand.
public export
data QuoteMode = StandardQuotes | BackslashEscapes

||| Ordinary characters cannot terminate a quoted body or start an escape.
public export
data Ordinary : QuoteMode -> Char -> Type where
  StandardChar : (c : Char) -> Not (c = '\'') -> Not (c = '\0') ->
                 Ordinary StandardQuotes c
  BackslashChar : (c : Char) -> Not (c = '\'') -> Not (c = '\0') ->
                  Not (c = '\\') -> Ordinary BackslashEscapes c

public export
data CharacterClass : QuoteMode -> Char -> Type where
  NullChar : CharacterClass mode '\0'
  QuoteChar : CharacterClass mode '\''
  SlashChar : CharacterClass BackslashEscapes '\\'
  PlainChar : Ordinary mode c -> CharacterClass mode c

public export
classify : (mode : QuoteMode) -> (c : Char) -> CharacterClass mode c
classify mode c = case decEq c '\0' of
  Yes equal => replace {p = CharacterClass mode} (sym equal) NullChar
  No nonNull => case decEq c '\'' of
    Yes equal => replace {p = CharacterClass mode} (sym equal) QuoteChar
    No nonQuote => case mode of
      StandardQuotes => PlainChar (StandardChar c nonQuote nonNull)
      BackslashEscapes => case decEq c '\\' of
        Yes equal => replace {p = CharacterClass BackslashEscapes} (sym equal) SlashChar
        No nonSlash => PlainChar (BackslashChar c nonQuote nonNull nonSlash)

||| A lexical encoding relation, including the legacy removal of NUL.
||| Each quote/slash pair denotes one input character; ordinary characters
||| retain their full value. There is no arbitrary "safe string" constructor.
public export
data BodyEncoding : QuoteMode -> List Char -> List Char -> Type where
  EndBody : BodyEncoding mode [] []
  DropNull : BodyEncoding mode input output ->
             BodyEncoding mode ('\0' :: input) output
  DoubleQuote : BodyEncoding mode input output ->
                BodyEncoding mode ('\'' :: input) ('\'' :: '\'' :: output)
  DoubleSlash : BodyEncoding BackslashEscapes input output ->
                BodyEncoding BackslashEscapes ('\\' :: input) ('\\' :: '\\' :: output)
  KeepChar : Ordinary mode c -> BodyEncoding mode input output ->
             BodyEncoding mode (c :: input) (c :: output)

||| The production encoder constructs the relation by structural recursion.
public export
encodeBody : (mode : QuoteMode) -> (input : List Char) ->
             (output ** BodyEncoding mode input output)
encodeBody mode [] = ([] ** EndBody)
encodeBody mode (c :: cs) = case classify mode c of
  NullChar => case encodeBody mode cs of
    (output ** evidence) => (output ** DropNull evidence)
  QuoteChar => case encodeBody mode cs of
    (output ** evidence) => ('\'' :: '\'' :: output ** DoubleQuote evidence)
  SlashChar => case encodeBody BackslashEscapes cs of
    (output ** evidence) => ('\\' :: '\\' :: output ** DoubleSlash evidence)
  PlainChar ordinary => case encodeBody mode cs of
    (output ** evidence) => (c :: output ** KeepChar ordinary evidence)

public export
bodyChars : QuoteMode -> List Char -> List Char
bodyChars mode input = fst (encodeBody mode input)

public export
bodyCorrect : (mode : QuoteMode) -> (input : List Char) ->
              BodyEncoding mode input (bodyChars mode input)
bodyCorrect mode input = snd (encodeBody mode input)

||| A separate grammar: every apostrophe occurs as part of a doubled pair.
public export
data PairedQuotes : List Char -> Type where
  NoQuotes : PairedQuotes []
  QuotePair : PairedQuotes rest -> PairedQuotes ('\'' :: '\'' :: rest)
  OtherChar : (c : Char) -> Not (c = '\'') -> PairedQuotes rest ->
              PairedQuotes (c :: rest)

ordinaryNotQuote : Ordinary mode c -> Not (c = '\'')
ordinaryNotQuote (StandardChar c nonQuote _) = nonQuote
ordinaryNotQuote (BackslashChar c nonQuote _ _) = nonQuote

slashNotQuote : '\\' = '\'' -> Void
slashNotQuote Refl impossible

public export
encodingQuotesPaired : BodyEncoding mode input output -> PairedQuotes output
encodingQuotesPaired EndBody = NoQuotes
encodingQuotesPaired (DropNull rest) = encodingQuotesPaired rest
encodingQuotesPaired (DoubleQuote rest) = QuotePair (encodingQuotesPaired rest)
encodingQuotesPaired (DoubleSlash rest) =
  OtherChar '\\' slashNotQuote (OtherChar '\\' slashNotQuote (encodingQuotesPaired rest))
encodingQuotesPaired (KeepChar (StandardChar c nonQuote _) rest) =
  OtherChar c nonQuote (encodingQuotesPaired rest)
encodingQuotesPaired (KeepChar (BackslashChar c nonQuote _ _) rest) =
  OtherChar c nonQuote (encodingQuotesPaired rest)

||| The old claim "contains no quotes" was false. A lone body quote is forbidden.
public export
singleQuoteNotPaired : PairedQuotes ['\''] -> Void
singleQuoteNotPaired (OtherChar '\'' nonQuote rest) = nonQuote Refl

||| Representation evidence ties the character grammar to the packed String.
||| String pack/unpack correctness is a runtime assumption, not a new axiom.
public export
data RenderedBody : QuoteMode -> List Char -> String -> Type where
  BodyRendered : (chars : List Char) -> BodyEncoding mode input chars ->
                 RenderedBody mode input (pack chars)

public export
data RenderedLiteral : QuoteMode -> List Char -> String -> Type where
  LiteralRendered : (body : List Char) -> BodyEncoding mode input body ->
                    RenderedLiteral mode input (pack ('\'' :: body ++ ['\'']))

public export
renderBody : QuoteMode -> String -> String
renderBody mode input = pack (bodyChars mode (unpack input))

public export
renderLiteral : QuoteMode -> String -> String
renderLiteral mode input = pack ('\'' :: bodyChars mode (unpack input) ++ ['\''])

public export
renderBodyCorrect : (mode : QuoteMode) -> (input : String) ->
                    RenderedBody mode (unpack input) (renderBody mode input)
renderBodyCorrect mode input = BodyRendered _ (bodyCorrect mode (unpack input))

public export
renderLiteralCorrect : (mode : QuoteMode) -> (input : String) ->
                       RenderedLiteral mode (unpack input) (renderLiteral mode input)
renderLiteralCorrect mode input = LiteralRendered _ (bodyCorrect mode (unpack input))
