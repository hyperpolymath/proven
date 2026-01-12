// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * SafeContentType - Content-Type operations that cannot crash.
 *
 * All operations handle MIME sniffing prevention and validate
 * media types without throwing exceptions.
 */

/** Media type category */
type mediaCategory =
  | TextMedia
  | ImageMedia
  | AudioMedia
  | VideoMedia
  | ApplicationMedia
  | MultipartMedia
  | MessageMedia
  | FontMedia
  | ModelMedia
  | CustomMedia

/** Charset encoding */
type charset =
  | UTF8
  | UTF16LE
  | UTF16BE
  | ISO8859_1
  | ASCII
  | Windows1252
  | OtherCharset(string)

/** Media type */
type mediaType = {
  mediaType: string,
  subtype: string,
  suffix: option<string>,
  category: mediaCategory,
}

/** Complete content type */
type contentType = {
  media: mediaType,
  charset: option<charset>,
  boundary: option<string>,
  params: array<(string, string)>,
}

/** Parse category from type string */
let parseCategory = (s: string): mediaCategory => {
  switch Js.String2.toLowerCase(s) {
  | "text" => TextMedia
  | "image" => ImageMedia
  | "audio" => AudioMedia
  | "video" => VideoMedia
  | "application" => ApplicationMedia
  | "multipart" => MultipartMedia
  | "message" => MessageMedia
  | "font" => FontMedia
  | "model" => ModelMedia
  | _ => CustomMedia
  }
}

/** Check if charset is Unicode */
let isUnicode = (c: charset): bool => {
  switch c {
  | UTF8 | UTF16LE | UTF16BE => true
  | _ => false
  }
}

/** Render charset to string */
let charsetToString = (c: charset): string => {
  switch c {
  | UTF8 => "utf-8"
  | UTF16LE => "utf-16le"
  | UTF16BE => "utf-16be"
  | ISO8859_1 => "iso-8859-1"
  | ASCII => "us-ascii"
  | Windows1252 => "windows-1252"
  | OtherCharset(s) => Js.String2.toLowerCase(s)
  }
}

/** Parse charset from string */
let parseCharset = (s: string): charset => {
  switch Js.String2.toLowerCase(Js.String2.trim(s)) {
  | "utf-8" | "utf8" => UTF8
  | "utf-16le" => UTF16LE
  | "utf-16be" => UTF16BE
  | "iso-8859-1" | "latin1" => ISO8859_1
  | "us-ascii" | "ascii" => ASCII
  | "windows-1252" | "cp1252" => Windows1252
  | other => OtherCharset(other)
  }
}

/** Valid token characters per RFC 2045 */
let isValidTokenChar = (c: string): bool => {
  let code = Js.String2.charCodeAt(c, 0)
  code >= 33.0 && code <= 126.0 && !Js.String2.includes("()<>@,;:\\\"/[]?=", c)
}

/** Types that can be sniffed to something dangerous */
let sniffableToDangerous = ["text/plain", "application/octet-stream", "application/x-unknown", "unknown/unknown"]

/** Check if type can be sniffed to dangerous */
let canSniffToDangerous = (s: string): bool => {
  Js.Array2.includes(sniffableToDangerous, Js.String2.toLowerCase(s))
}

/** Extract suffix from subtype (e.g., "json" from "vnd.api+json") */
let extractSuffix = (subtype: string): (string, option<string>) => {
  switch Js.String2.indexOf(subtype, "+") {
  | -1 => (subtype, None)
  | idx => (
      Js.String2.slice(subtype, ~from=0, ~to_=idx),
      Some(Js.String2.sliceToEnd(subtype, ~from=idx + 1)),
    )
  }
}

/** Parse Content-Type header */
let parse = (raw: string): option<contentType> => {
  let trimmed = Js.String2.trim(raw)
  if Js.String2.length(trimmed) == 0 || Js.String2.length(trimmed) > 1024 {
    None
  } else {
    let parts = Js.String2.split(trimmed, ";")
    switch Js.Array2.shift(parts) {
    | None => None
    | Some(mediaStr) =>
      switch Js.String2.indexOf(mediaStr, "/") {
      | -1 => None
      | idx =>
        let t = Js.String2.toLowerCase(Js.String2.trim(Js.String2.slice(mediaStr, ~from=0, ~to_=idx)))
        let s = Js.String2.toLowerCase(Js.String2.trim(Js.String2.sliceToEnd(mediaStr, ~from=idx + 1)))

        if Js.String2.length(t) == 0 || Js.String2.length(s) == 0 {
          None
        } else {
          let (baseSubtype, suffix) = extractSuffix(s)
          let category = parseCategory(t)
          let media = {mediaType: t, subtype: baseSubtype, suffix: suffix, category: category}

          // Parse parameters
          let charset = ref(None)
          let boundary = ref(None)
          let otherParams = []

          Js.Array2.forEach(parts, param => {
            let p = Js.String2.trim(param)
            switch Js.String2.indexOf(p, "=") {
            | -1 => ()
            | eqIdx =>
              let name = Js.String2.toLowerCase(Js.String2.trim(Js.String2.slice(p, ~from=0, ~to_=eqIdx)))
              let value = Js.String2.trim(Js.String2.sliceToEnd(p, ~from=eqIdx + 1))
              // Remove quotes if present
              let unquoted = if Js.String2.startsWith(value, "\"") && Js.String2.endsWith(value, "\"") {
                Js.String2.slice(value, ~from=1, ~to_=Js.String2.length(value) - 1)
              } else {
                value
              }

              switch name {
              | "charset" => charset := Some(parseCharset(unquoted))
              | "boundary" => boundary := Some(unquoted)
              | _ => Js.Array2.push(otherParams, (name, unquoted))->ignore
              }
            }
          })

          Some({
            media: media,
            charset: charset.contents,
            boundary: boundary.contents,
            params: otherParams,
          })
        }
      }
    }
  }
}

/** Create content type from type and subtype */
let make = (t: string, s: string): option<contentType> => {
  let lower_t = Js.String2.toLowerCase(Js.String2.trim(t))
  let lower_s = Js.String2.toLowerCase(Js.String2.trim(s))

  if Js.String2.length(lower_t) == 0 || Js.String2.length(lower_s) == 0 {
    None
  } else if Js.String2.length(lower_t) > 127 || Js.String2.length(lower_s) > 127 {
    None
  } else {
    let (baseSubtype, suffix) = extractSuffix(lower_s)
    let category = parseCategory(lower_t)
    Some({
      media: {mediaType: lower_t, subtype: baseSubtype, suffix: suffix, category: category},
      charset: None,
      boundary: None,
      params: [],
    })
  }
}

/** Render content type to string */
let render = (ct: contentType): string => {
  let base =
    ct.media.mediaType ++
    "/" ++
    ct.media.subtype ++
    switch ct.media.suffix {
    | Some(s) => "+" ++ s
    | None => ""
    }

  let withCharset = switch ct.charset {
  | Some(c) => base ++ "; charset=" ++ charsetToString(c)
  | None => base
  }

  let withBoundary = switch ct.boundary {
  | Some(b) => withCharset ++ "; boundary=" ++ b
  | None => withCharset
  }

  Js.Array2.reduce(ct.params, (acc, (name, value)) => acc ++ "; " ++ name ++ "=" ++ value, withBoundary)
}

/** Add charset to content type */
let withCharset = (c: charset, ct: contentType): contentType => {
  {...ct, charset: Some(c)}
}

/** Add boundary to content type */
let withBoundary = (b: string, ct: contentType): contentType => {
  {...ct, boundary: Some(b)}
}

/** Check if content type is text */
let isText = (ct: contentType): bool => ct.media.category == TextMedia

/** Check if content type is JSON */
let isJson = (ct: contentType): bool => {
  ct.media.subtype == "json" || ct.media.suffix == Some("json")
}

/** Check if content type is XML */
let isXml = (ct: contentType): bool => {
  ct.media.subtype == "xml" || ct.media.suffix == Some("xml")
}

/** Check if content type is HTML */
let isHtml = (ct: contentType): bool => {
  ct.media.mediaType == "text" && ct.media.subtype == "html"
}

/** Check if content type is multipart */
let isMultipart = (ct: contentType): bool => ct.media.category == MultipartMedia

/** Check if safe from MIME sniffing */
let safeFromSniffing = (ct: contentType): bool => {
  !canSniffToDangerous(ct.media.mediaType ++ "/" ++ ct.media.subtype)
}

// Common content types
let textPlain = {
  media: {mediaType: "text", subtype: "plain", suffix: None, category: TextMedia},
  charset: Some(UTF8),
  boundary: None,
  params: [],
}

let textHtml = {
  media: {mediaType: "text", subtype: "html", suffix: None, category: TextMedia},
  charset: Some(UTF8),
  boundary: None,
  params: [],
}

let applicationJson = {
  media: {mediaType: "application", subtype: "json", suffix: None, category: ApplicationMedia},
  charset: Some(UTF8),
  boundary: None,
  params: [],
}

let applicationXml = {
  media: {mediaType: "application", subtype: "xml", suffix: None, category: ApplicationMedia},
  charset: Some(UTF8),
  boundary: None,
  params: [],
}

let applicationOctetStream = {
  media: {mediaType: "application", subtype: "octet-stream", suffix: None, category: ApplicationMedia},
  charset: None,
  boundary: None,
  params: [],
}

let formUrlencoded = {
  media: {mediaType: "application", subtype: "x-www-form-urlencoded", suffix: None, category: ApplicationMedia},
  charset: None,
  boundary: None,
  params: [],
}
