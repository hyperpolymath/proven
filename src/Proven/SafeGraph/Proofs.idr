-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
||| Safety proofs for SafeGraph weighted-graph surface.
module Proven.SafeGraph.Proofs

import Proven.SafeGraph

%default total

public export
mkEdgeSource : (s, t : v) -> (w : weight) -> (MkEdge s t w).source = s
mkEdgeSource s t w = Refl

public export
mkEdgeTarget : (s, t : v) -> (w : weight) -> (MkEdge s t w).target = t
mkEdgeTarget s t w = Refl

public export
mkEdgeWeight : (s, t : v) -> (w : weight) -> (MkEdge s t w).weight = w
mkEdgeWeight s t w = Refl

public export
mkGraphVertices :
  (vs : List v) -> (es : List (Edge v w))
  -> (MkGraph vs es).vertices = vs
mkGraphVertices vs es = Refl

public export
mkGraphEdges :
  (vs : List v) -> (es : List (Edge v w))
  -> (MkGraph vs es).edges = es
mkGraphEdges vs es = Refl

public export
mkUndirectedGraph :
  (g : Graph v w) -> (MkUndirected g).graph = g
mkUndirectedGraph g = Refl
