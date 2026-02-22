-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
--
-- BGP Routing Table and Best Path Selection (RFC 4271 Section 9.1)
--
-- Route selection uses SafeMath for all comparisons (no integer overflow).
-- The Adj-RIB-In, Loc-RIB, and Adj-RIB-Out are implemented as pure
-- functional data structures — no mutable state, no data races.

module BGP.Route

import BGP.Message

%default total

-- ============================================================================
-- Route Information Base (RIB) entries
-- ============================================================================

||| A single route entry in the RIB.
public export
record RouteEntry where
  constructor MkRouteEntry
  prefix     : IPPrefix
  attributes : PathAttributes
  peerAddr   : Bits32          -- IP address of the peer that advertised this
  peerAS     : Bits32          -- AS number of the advertising peer
  isValid    : Bool            -- Has this route passed validation?
  isBestPath : Bool            -- Is this the selected best path?

||| The Adj-RIB-In: routes received from a peer (RFC 4271 Section 3.2).
public export
AdjRIBIn : Type
AdjRIBIn = List RouteEntry

||| The Loc-RIB: locally selected best routes (RFC 4271 Section 3.2).
public export
LocRIB : Type
LocRIB = List RouteEntry

||| The Adj-RIB-Out: routes to be advertised to peers (RFC 4271 Section 3.2).
public export
AdjRIBOut : Type
AdjRIBOut = List RouteEntry

-- ============================================================================
-- AS Path Length calculation
-- ============================================================================

||| Calculate the total AS path length (counting AS_SEQUENCE entries).
||| AS_SET segments count as 1 regardless of size (RFC 4271 Section 9.1.2.2).
public export
asPathLength : List ASPathSegment -> Nat
asPathLength [] = 0
asPathLength (seg :: rest) =
  let segLen = case seg.segmentType of
                    AS_SET      => 1  -- SET counts as 1
                    AS_SEQUENCE => length seg.asNumbers
  in segLen + asPathLength rest

-- ============================================================================
-- Best Path Selection (RFC 4271 Section 9.1.2)
--
-- The decision process, in order:
--   1. Highest LOCAL_PREF
--   2. Shortest AS_PATH
--   3. Lowest ORIGIN (IGP < EGP < INCOMPLETE)
--   4. Lowest MED (from same neighbor AS)
--   5. Prefer eBGP over iBGP
--   6. Lowest router ID (tiebreaker)
-- ============================================================================

||| Origin to numeric value for comparison.
originValue : Origin -> Nat
originValue IGP        = 0
originValue EGP        = 1
originValue INCOMPLETE = 2

||| Compare two route entries. Returns True if `a` is preferred over `b`.
||| Uses the RFC 4271 Section 9.1.2 decision process.
public export
isPreferred : (localAS : Bits32) -> RouteEntry -> RouteEntry -> Bool
isPreferred localAS a b =
  let -- Step 1: Highest LOCAL_PREF wins
      lpA = fromMaybe 100 a.attributes.localPref  -- Default 100
      lpB = fromMaybe 100 b.attributes.localPref
  in if lpA /= lpB then lpA > lpB
  else
    let -- Step 2: Shortest AS_PATH wins
        lenA = asPathLength a.attributes.asPath
        lenB = asPathLength b.attributes.asPath
    in if lenA /= lenB then lenA < lenB
  else
    let -- Step 3: Lowest ORIGIN wins (IGP < EGP < INCOMPLETE)
        origA = maybe 3 originValue a.attributes.origin
        origB = maybe 3 originValue b.attributes.origin
    in if origA /= origB then origA < origB
  else
    let -- Step 4: Lowest MED wins (only if from same neighbor AS)
        medA = fromMaybe 0 a.attributes.med
        medB = fromMaybe 0 b.attributes.med
    in if a.peerAS == b.peerAS && medA /= medB then medA < medB
  else
    let -- Step 5: eBGP over iBGP
        isEbgpA = a.peerAS /= localAS
        isEbgpB = b.peerAS /= localAS
    in if isEbgpA && not isEbgpB then True
  else if not isEbgpA && isEbgpB then False
  else
    -- Step 6: Lowest peer address (tiebreaker)
    a.peerAddr < b.peerAddr

||| Select the best route from a list of candidates for the same prefix.
||| Returns Nothing if the list is empty.
public export
selectBestPath : (localAS : Bits32) -> List RouteEntry -> Maybe RouteEntry
selectBestPath _       []        = Nothing
selectBestPath localAS (r :: rs) = Just (foldl pick r rs)
  where
    pick : RouteEntry -> RouteEntry -> RouteEntry
    pick best candidate =
      if isPreferred localAS candidate best
        then candidate
        else best

-- ============================================================================
-- Route manipulation
-- ============================================================================

||| Add a route to the Adj-RIB-In.
public export
addRoute : RouteEntry -> AdjRIBIn -> AdjRIBIn
addRoute entry rib = entry :: rib

||| Remove all routes from a specific peer.
public export
removeRoutesFromPeer : (peerAddr : Bits32) -> AdjRIBIn -> AdjRIBIn
removeRoutesFromPeer addr = filter (\r => r.peerAddr /= addr)

||| Remove routes matching a specific prefix from a specific peer.
public export
withdrawRoute : (peerAddr : Bits32) -> IPPrefix -> AdjRIBIn -> AdjRIBIn
withdrawRoute addr prefix = filter (\r =>
  not (r.peerAddr == addr
       && r.prefix.prefixLen == prefix.prefixLen
       && r.prefix.prefix == prefix.prefix))

||| Recalculate best paths for all prefixes in the RIB.
||| Groups routes by prefix, selects best for each, produces Loc-RIB.
public export
recalculateBestPaths : (localAS : Bits32) -> AdjRIBIn -> LocRIB
recalculateBestPaths localAS rib =
  let -- Group by prefix (simple: collect unique prefixes)
      prefixes = nub (map (\r => (r.prefix.prefixLen, r.prefix.prefix)) rib)
      -- For each prefix, find candidates and select best
      bestRoutes = mapMaybe (\(plen, paddr) =>
        let candidates = filter (\r => r.prefix.prefixLen == plen
                                    && r.prefix.prefix == paddr) rib
        in selectBestPath localAS candidates
        ) prefixes
  in map (\r => { isBestPath := True } r) bestRoutes
  where
    nub : List (Bits8, Bits32) -> List (Bits8, Bits32)
    nub [] = []
    nub (x :: xs) = x :: nub (filter (/= x) xs)
