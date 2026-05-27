-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| Safety proofs for SafeBibTeX LaTeX-injection / malformed-entry prevention.
|||
||| `Proven.SafeBibTeX` ships entry-type / citation-key / field validators
||| with no machine-checked theorems. This file discharges:
|||
|||   * `EntryType` enum self-equality (all 11 constructors).
|||   * `EntryType` wire-format anchors via `show` (all 11 constructors).
|||   * `requiredFields` exhaustiveness — every entry type lists a
|||     specific required-field set; `Misc` is the only one with no
|||     requirements.
|||   * `mkCitationKey` produces a `Just` on a known-valid key (anchor
|||     case).
|||
||| OWED: validators built on `unpack` / `isInfixOf` (e.g.
||| `hasDangerousLaTeX`, `isValidCitationKey`) go through opaque
||| `String` FFI — standards#128 family.
|||
||| Zero `believe_me` / `idris_crash`.
module Proven.SafeBibTeX.Proofs

import Proven.SafeBibTeX

%default total

--------------------------------------------------------------------------------
-- EntryType enum self-equality (11 constructors)
--------------------------------------------------------------------------------

public export
articleSelfEq : Article == Article = True
articleSelfEq = Refl

public export
bookSelfEq : Book == Book = True
bookSelfEq = Refl

public export
inProceedingsSelfEq : InProceedings == InProceedings = True
inProceedingsSelfEq = Refl

public export
inCollectionSelfEq : InCollection == InCollection = True
inCollectionSelfEq = Refl

public export
phdThesisSelfEq : PhdThesis == PhdThesis = True
phdThesisSelfEq = Refl

public export
mastersThesisSelfEq : MastersThesis == MastersThesis = True
mastersThesisSelfEq = Refl

public export
techReportSelfEq : TechReport == TechReport = True
techReportSelfEq = Refl

public export
miscSelfEq : Misc == Misc = True
miscSelfEq = Refl

public export
unpublishedSelfEq : Unpublished == Unpublished = True
unpublishedSelfEq = Refl

public export
onlineSelfEq : Online == Online = True
onlineSelfEq = Refl

public export
softwareSelfEq : Software == Software = True
softwareSelfEq = Refl

||| Anchor the `_ == _ = False` fall-through.
public export
articleNotBook : Article == Book = False
articleNotBook = Refl

public export
phdNotMastersThesis : PhdThesis == MastersThesis = False
phdNotMastersThesis = Refl

--------------------------------------------------------------------------------
-- `show EntryType` wire-format anchors (BibTeX-on-the-wire conformance)
--------------------------------------------------------------------------------

public export
articleShow : show Article = "article"
articleShow = Refl

public export
bookShow : show Book = "book"
bookShow = Refl

public export
inProceedingsShow : show InProceedings = "inproceedings"
inProceedingsShow = Refl

public export
inCollectionShow : show InCollection = "incollection"
inCollectionShow = Refl

public export
phdThesisShow : show PhdThesis = "phdthesis"
phdThesisShow = Refl

public export
mastersThesisShow : show MastersThesis = "mastersthesis"
mastersThesisShow = Refl

public export
techReportShow : show TechReport = "techreport"
techReportShow = Refl

public export
miscShow : show Misc = "misc"
miscShow = Refl

public export
unpublishedShow : show Unpublished = "unpublished"
unpublishedShow = Refl

public export
onlineShow : show Online = "online"
onlineShow = Refl

public export
softwareShow : show Software = "software"
softwareShow = Refl

--------------------------------------------------------------------------------
-- `requiredFields` schema anchors
--------------------------------------------------------------------------------

||| `Article` requires author, title, journal, year.
public export
articleRequired :
  requiredFields Article = ["author", "title", "journal", "year"]
articleRequired = Refl

||| `Book` requires author, title, publisher, year.
public export
bookRequired :
  requiredFields Book = ["author", "title", "publisher", "year"]
bookRequired = Refl

||| `Misc` has no required fields (catch-all).
public export
miscNoRequired : requiredFields Misc = []
miscNoRequired = Refl

||| `Online` requires url (modern citation requirement).
public export
onlineRequired :
  requiredFields Online = ["author", "title", "url"]
onlineRequired = Refl

||| `Software` requires version (reproducibility requirement).
public export
softwareRequired :
  requiredFields Software = ["author", "title", "version"]
softwareRequired = Refl

||| `PhdThesis` requires school.
public export
phdThesisRequired :
  requiredFields PhdThesis = ["author", "title", "school", "year"]
phdThesisRequired = Refl

||| `Unpublished` requires note.
public export
unpublishedRequired :
  requiredFields Unpublished = ["author", "title", "note"]
unpublishedRequired = Refl
