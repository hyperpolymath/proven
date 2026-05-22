-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| Safety proofs for SafeMarkdown rendering surface.
|||
||| HeadingLevel / ListStyle / Alignment have no Eq instance;
||| use `headingNum` direct anchors instead.
module Proven.SafeMarkdown.Proofs

import Proven.SafeMarkdown

%default total

public export
h1IsOne : headingNum H1 = 1
h1IsOne = Refl

public export
h2IsTwo : headingNum H2 = 2
h2IsTwo = Refl

public export
h3IsThree : headingNum H3 = 3
h3IsThree = Refl

public export
h4IsFour : headingNum H4 = 4
h4IsFour = Refl

public export
h5IsFive : headingNum H5 = 5
h5IsFive = Refl

public export
h6IsSix : headingNum H6 = 6
h6IsSix = Refl
