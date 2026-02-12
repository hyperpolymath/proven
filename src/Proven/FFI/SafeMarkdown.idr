-- SPDX-License-Identifier: Apache-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| FFI exports for SafeMarkdown operations (66/75)
module Proven.FFI.SafeMarkdown

import Proven.SafeMarkdown
import Proven.Core

%default total

encodeBool : Bool -> Int
encodeBool False = 0
encodeBool True = 1

-- Heading levels: H1=1, H2=2, H3=3, H4=4, H5=5, H6=6
encodeHeadingLevel : HeadingLevel -> Int
encodeHeadingLevel H1 = 1
encodeHeadingLevel H2 = 2
encodeHeadingLevel H3 = 3
encodeHeadingLevel H4 = 4
encodeHeadingLevel H5 = 5
encodeHeadingLevel H6 = 6

-- List styles: Unordered=0, Ordered=1
encodeListStyle : ListStyle -> Int
encodeListStyle Unordered = 0
encodeListStyle Ordered = 1

-- Table alignment: AlignLeft=0, AlignCenter=1, AlignRight=2, AlignDefault=3
encodeAlignment : Alignment -> Int
encodeAlignment AlignLeft = 0
encodeAlignment AlignCenter = 1
encodeAlignment AlignRight = 2
encodeAlignment AlignDefault = 3

export
proven_idris_md_heading_valid : Int -> Int
proven_idris_md_heading_valid level = encodeBool (level >= 1 && level <= 6)

export
proven_idris_md_needs_escape : Int -> Int
proven_idris_md_needs_escape charCode =
  let c = charCode
  in encodeBool (c == 92 || c == 96 || c == 42 || c == 95 || c == 123 || c == 125 ||
                 c == 91 || c == 93 || c == 40 || c == 41 || c == 35 || c == 43 ||
                 c == 45 || c == 46 || c == 33 || c == 124)
