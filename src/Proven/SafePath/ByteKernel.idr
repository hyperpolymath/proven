-- SPDX-License-Identifier: MPL-2.0
module Proven.SafePath.ByteKernel

%default total

||| Raw-byte lexical policy. NUL is a boundary, not end-of-input.
||| This is not URL decoding, path normalization or filesystem confinement.
public export
data ByteClass = Boundary | Dot | DataByte

public export
classify : Bits8 -> ByteClass
classify b = if b == 0 || b == 47 || b == 92 then Boundary
             else if b == 46 then Dot else DataByte

public export
data ScanState = Start | OneDot | TwoDots | Other | Found

public export
next : ScanState -> ByteClass -> ScanState
next Found _ = Found
next Start Boundary = Start
next Start Dot = OneDot
next Start DataByte = Other
next OneDot Boundary = Start
next OneDot Dot = TwoDots
next OneDot DataByte = Other
next TwoDots Boundary = Found
next TwoDots _ = Other
next Other Boundary = Start
next Other _ = Other

public export
finish : ScanState -> Bool
finish TwoDots = True
finish Found = True
finish _ = False

public export
encodeState : ScanState -> Bits8
encodeState Start = 0
encodeState OneDot = 1
encodeState TwoDots = 2
encodeState Other = 3
encodeState Found = 4

public export
decodeState : Bits8 -> Maybe ScanState
decodeState s = if s == 0 then Just Start else if s == 1 then Just OneDot
               else if s == 2 then Just TwoDots else if s == 3 then Just Other
               else if s == 4 then Just Found else Nothing

public export
boolCode : Bool -> Bits8
boolCode False = 0
boolCode True = 1

||| Production RefC entrypoints. Invalid protocol states return 255.
public export
%noinline
pathStep : Bits8 -> Bits8 -> Bits8
pathStep s b = case decodeState s of
  Just state => encodeState (next state (classify b))
  Nothing => 255

public export
%noinline
pathFinish : Bits8 -> Bits8
pathFinish s = case decodeState s of
  Just state => boolCode (finish state)
  Nothing => 255

public export
stepEncodes : (s : ScanState) -> (b : Bits8) ->
  pathStep (encodeState s) b = encodeState (next s (classify b))
stepEncodes Start b = Refl
stepEncodes OneDot b = Refl
stepEncodes TwoDots b = Refl
stepEncodes Other b = Refl
stepEncodes Found b = Refl

public export
finishEncodes : (s : ScanState) ->
  pathFinish (encodeState s) = boolCode (finish s)
finishEncodes Start = Refl
finishEncodes OneDot = Refl
finishEncodes TwoDots = Refl
finishEncodes Other = Refl
finishEncodes Found = Refl

public export
scanTokens : ScanState -> List ByteClass -> ScanState
scanTokens s [] = s
scanTokens s (b :: bs) = scanTokens (next s b) bs

public export
scanBytes : Bits8 -> List Bits8 -> Bits8
scanBytes s [] = s
scanBytes s (b :: bs) = scanBytes (pathStep s b) bs

public export
scanEncodes : (s : ScanState) -> (bs : List Bits8) ->
  scanBytes (encodeState s) bs = encodeState (scanTokens s (map Proven.SafePath.ByteKernel.classify bs))
scanEncodes s [] = Refl
scanEncodes s (b :: bs) =
  rewrite stepEncodes s b in scanEncodes (next s (classify b)) bs

public export
detect : List Bits8 -> Bool
detect bs = finish (scanTokens Start (map classify bs))

||| The actual generated step/finish protocol refines the typed policy.
public export
wireRefinesDetection : (bs : List Bits8) ->
  pathFinish (scanBytes 0 bs) = boolCode (detect bs)
wireRefinesDetection bs =
  rewrite scanEncodes Start bs in finishEncodes (scanTokens Start (map classify bs))

public export
scanAppend : (s : ScanState) -> (xs, ys : List ByteClass) ->
  scanTokens s (xs ++ ys) = scanTokens (scanTokens s xs) ys
scanAppend s [] ys = Refl
scanAppend s (x :: xs) ys = scanAppend (next s x) xs ys

public export
foundStaysFound : (bs : List ByteClass) -> scanTokens Found bs = Found
foundStaysFound [] = Refl
foundStaysFound (b :: bs) = foundStaysFound bs

public export
parentAfterState : (s : ScanState) -> (after : List ByteClass) ->
  scanTokens s (Boundary :: Dot :: Dot :: Boundary :: after) = Found
parentAfterState Start after = foundStaysFound after
parentAfterState OneDot after = foundStaysFound after
parentAfterState TwoDots after = foundStaysFound after
parentAfterState Other after = foundStaysFound after
parentAfterState Found after = foundStaysFound after

||| Every complete parent component is detected in arbitrary surrounding data.
public export
parentInContext : (before, after : List ByteClass) ->
  scanTokens Start (before ++ (Boundary :: Dot :: Dot :: Boundary :: after)) = Found
parentInContext before after =
  rewrite scanAppend Start before (Boundary :: Dot :: Dot :: Boundary :: after)
    in parentAfterState (scanTokens Start before) after

public export
invalidStateRejected : (s, b : Bits8) -> decodeState s = Nothing ->
  pathStep s b = 255
invalidStateRejected s b invalid = rewrite invalid in Refl

public export
invalidFinishRejected : (s : Bits8) -> decodeState s = Nothing -> pathFinish s = 255
invalidFinishRejected s invalid = rewrite invalid in Refl

public export
backslashIsBoundary : classify 92 = Boundary
backslashIsBoundary = Refl

public export
nulDoesNotTerminate : detect [0, 46, 46, 92] = True
nulDoesNotTerminate = Refl

public export
tripleDotIsNotParent : detect [46, 46, 46] = False
tripleDotIsNotParent = Refl
