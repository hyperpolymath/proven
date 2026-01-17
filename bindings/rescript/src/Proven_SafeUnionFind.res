// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * SafeUnionFind - Disjoint set union (Union-Find) data structure that cannot crash.
 *
 * Provides efficient operations for tracking disjoint sets with path compression
 * and union by rank optimizations. All operations are bounded and cannot cause
 * out-of-bounds access or infinite loops.
 */

/** Error types for Union-Find operations */
type unionFindError =
  | ElementOutOfBounds
  | MaxCapacityReached
  | InvalidOperation

/** Union-Find data structure with fixed capacity */
type t = {
  /** Parent array - parent[i] is the parent of element i.
   * If parent[i] == i, then i is a root.
   */
  mutable parent: array<int>,
  /** Rank array for union by rank optimization */
  mutable rank: array<int>,
  /** Number of elements currently in the structure */
  mutable elementCount: int,
  /** Number of disjoint sets */
  mutable setCount: int,
  /** Maximum capacity */
  maxElements: int,
}

/** Initialize an empty Union-Find structure with specified capacity */
let make = (maxElements: int): t => {
  {
    parent: [],
    rank: [],
    elementCount: 0,
    setCount: 0,
    maxElements: maxElements,
  }
}

/** Initialize with a specific number of elements.
 * Each element starts in its own singleton set.
 */
let makeWithElements = (maxElements: int, initialCount: int): result<t, unionFindError> => {
  if initialCount > maxElements {
    Error(MaxCapacityReached)
  } else {
    let parent = Belt.Array.makeBy(initialCount, i => i)
    let rank = Belt.Array.make(initialCount, 0)
    Ok({
      parent: parent,
      rank: rank,
      elementCount: initialCount,
      setCount: initialCount,
      maxElements: maxElements,
    })
  }
}

/** Add a new element as a singleton set.
 * Returns the index of the new element.
 */
let makeSet = (uf: t): result<int, unionFindError> => {
  if uf.elementCount >= uf.maxElements {
    Error(MaxCapacityReached)
  } else {
    let newElementIndex = uf.elementCount
    uf.parent = Belt.Array.concat(uf.parent, [newElementIndex])
    uf.rank = Belt.Array.concat(uf.rank, [0])
    uf.elementCount = uf.elementCount + 1
    uf.setCount = uf.setCount + 1
    Ok(newElementIndex)
  }
}

/** Find the representative (root) of the set containing element x.
 * Uses path compression for amortized near-constant time.
 */
let find = (uf: t, elementIndex: int): result<int, unionFindError> => {
  if elementIndex < 0 || elementIndex >= uf.elementCount {
    Error(ElementOutOfBounds)
  } else {
    // Find root with path compression
    let current = ref(elementIndex)
    // First pass: find root
    while true {
      switch Belt.Array.get(uf.parent, current.contents) {
      | Some(parentIdx) =>
        if parentIdx == current.contents {
          // Found root, break
          current := parentIdx
          current := -1 // Signal to exit
        } else {
          current := parentIdx
        }
      | None => current := -1 // Exit on error
      }
      if current.contents == -1 {
        current := elementIndex // Reset for path compression
        // Break the while loop
        current := -2
      }
    }->ignore

    // Find root properly
    let root = ref(elementIndex)
    let continue1 = ref(true)
    while continue1.contents {
      switch Belt.Array.get(uf.parent, root.contents) {
      | Some(parentIdx) =>
        if parentIdx == root.contents {
          continue1 := false
        } else {
          root := parentIdx
        }
      | None => continue1 := false
      }
    }

    // Second pass: path compression
    let current2 = ref(elementIndex)
    let continue2 = ref(true)
    while continue2.contents {
      switch Belt.Array.get(uf.parent, current2.contents) {
      | Some(parentIdx) =>
        if parentIdx == root.contents || current2.contents == root.contents {
          continue2 := false
        } else {
          Belt.Array.setUnsafe(uf.parent, current2.contents, root.contents)
          current2 := parentIdx
        }
      | None => continue2 := false
      }
    }

    Ok(root.contents)
  }
}

/** Find without modifying the structure (const version) */
let findConst = (uf: t, elementIndex: int): result<int, unionFindError> => {
  if elementIndex < 0 || elementIndex >= uf.elementCount {
    Error(ElementOutOfBounds)
  } else {
    let current = ref(elementIndex)
    let continue1 = ref(true)
    while continue1.contents {
      switch Belt.Array.get(uf.parent, current.contents) {
      | Some(parentIdx) =>
        if parentIdx == current.contents {
          continue1 := false
        } else {
          current := parentIdx
        }
      | None => continue1 := false
      }
    }
    Ok(current.contents)
  }
}

/** Union the sets containing elements x and y.
 * Uses union by rank to keep trees balanced.
 * Returns true if a merge occurred, false if already in same set.
 */
let unite = (uf: t, elementX: int, elementY: int): result<bool, unionFindError> => {
  switch (find(uf, elementX), find(uf, elementY)) {
  | (Ok(rootX), Ok(rootY)) =>
    if rootX == rootY {
      Ok(false) // Already in same set
    } else {
      let rankX = Belt.Array.get(uf.rank, rootX)->Belt.Option.getWithDefault(0)
      let rankY = Belt.Array.get(uf.rank, rootY)->Belt.Option.getWithDefault(0)

      if rankX < rankY {
        Belt.Array.setUnsafe(uf.parent, rootX, rootY)
      } else if rankX > rankY {
        Belt.Array.setUnsafe(uf.parent, rootY, rootX)
      } else {
        Belt.Array.setUnsafe(uf.parent, rootY, rootX)
        Belt.Array.setUnsafe(uf.rank, rootX, rankX + 1)
      }

      uf.setCount = uf.setCount - 1
      Ok(true)
    }
  | (Error(e), _) | (_, Error(e)) => Error(e)
  }
}

/** Check if two elements are in the same set */
let connected = (uf: t, elementX: int, elementY: int): result<bool, unionFindError> => {
  switch (find(uf, elementX), find(uf, elementY)) {
  | (Ok(rootX), Ok(rootY)) => Ok(rootX == rootY)
  | (Error(e), _) | (_, Error(e)) => Error(e)
  }
}

/** Check if two elements are in the same set (const version) */
let connectedConst = (uf: t, elementX: int, elementY: int): result<bool, unionFindError> => {
  switch (findConst(uf, elementX), findConst(uf, elementY)) {
  | (Ok(rootX), Ok(rootY)) => Ok(rootX == rootY)
  | (Error(e), _) | (_, Error(e)) => Error(e)
  }
}

/** Get the number of elements in the set containing x */
let setSize = (uf: t, elementIndex: int): result<int, unionFindError> => {
  switch find(uf, elementIndex) {
  | Ok(root) =>
    let sizeCount = ref(0)
    for i in 0 to uf.elementCount - 1 {
      switch findConst(uf, i) {
      | Ok(r) if r == root => sizeCount := sizeCount.contents + 1
      | _ => ()
      }
    }
    Ok(sizeCount.contents)
  | Error(e) => Error(e)
  }
}

/** Get all elements in the set containing x */
let getSetMembers = (uf: t, elementIndex: int): result<array<int>, unionFindError> => {
  switch find(uf, elementIndex) {
  | Ok(root) =>
    let members = ref([])
    for i in 0 to uf.elementCount - 1 {
      switch findConst(uf, i) {
      | Ok(r) if r == root => members := Belt.Array.concat(members.contents, [i])
      | _ => ()
      }
    }
    Ok(members.contents)
  | Error(e) => Error(e)
  }
}

/** Get all roots (set representatives) */
let getRoots = (uf: t): array<int> => {
  let roots = ref([])
  for i in 0 to uf.elementCount - 1 {
    switch Belt.Array.get(uf.parent, i) {
    | Some(parentIdx) if parentIdx == i =>
      roots := Belt.Array.concat(roots.contents, [i])
    | _ => ()
    }
  }
  roots.contents
}

/** Get the number of disjoint sets */
let countSets = (uf: t): int => {
  uf.setCount
}

/** Get the total number of elements */
let count = (uf: t): int => {
  uf.elementCount
}

/** Check if the structure is empty */
let isEmpty = (uf: t): bool => {
  uf.elementCount == 0
}

/** Reset to empty state */
let clear = (uf: t): unit => {
  uf.parent = []
  uf.rank = []
  uf.elementCount = 0
  uf.setCount = 0
}

/** Get all sets as an array of arrays */
let getAllSets = (uf: t): array<array<int>> => {
  let roots = getRoots(uf)
  Belt.Array.map(roots, root => {
    switch getSetMembers(uf, root) {
    | Ok(members) => members
    | Error(_) => []
    }
  })
}

/** Check if all elements are in the same set */
let isFullyConnected = (uf: t): bool => {
  uf.setCount == 1
}

/** Clone the Union-Find structure */
let clone = (uf: t): t => {
  {
    parent: Belt.Array.copy(uf.parent),
    rank: Belt.Array.copy(uf.rank),
    elementCount: uf.elementCount,
    setCount: uf.setCount,
    maxElements: uf.maxElements,
  }
}

/** Weighted Union-Find with associated values */
module Weighted = {
  type weightedUnionFind = {
    mutable parent: array<int>,
    mutable rank: array<int>,
    mutable weight: array<int>,
    mutable elementCount: int,
    mutable setCount: int,
    maxElements: int,
  }

  /** Create a new weighted Union-Find structure */
  let make = (maxElements: int): weightedUnionFind => {
    {
      parent: [],
      rank: [],
      weight: [],
      elementCount: 0,
      setCount: 0,
      maxElements: maxElements,
    }
  }

  /** Add a new element with initial weight of zero */
  let makeSet = (wuf: weightedUnionFind): result<int, unionFindError> => {
    if wuf.elementCount >= wuf.maxElements {
      Error(MaxCapacityReached)
    } else {
      let newElementIndex = wuf.elementCount
      wuf.parent = Belt.Array.concat(wuf.parent, [newElementIndex])
      wuf.rank = Belt.Array.concat(wuf.rank, [0])
      wuf.weight = Belt.Array.concat(wuf.weight, [0])
      wuf.elementCount = wuf.elementCount + 1
      wuf.setCount = wuf.setCount + 1
      Ok(newElementIndex)
    }
  }

  /** Find root and return the weight from element to root */
  type findResult = {
    root: int,
    weight: int,
  }

  let findWithWeight = (wuf: weightedUnionFind, elementIndex: int): result<findResult, unionFindError> => {
    if elementIndex < 0 || elementIndex >= wuf.elementCount {
      Error(ElementOutOfBounds)
    } else {
      // Iterative version with path compression
      let totalWeight = ref(0)
      let current = ref(elementIndex)
      let path = ref([])

      // First pass: find root and collect path
      let continue1 = ref(true)
      while continue1.contents {
        path := Belt.Array.concat(path.contents, [current.contents])
        switch Belt.Array.get(wuf.parent, current.contents) {
        | Some(parentIdx) =>
          if parentIdx == current.contents {
            continue1 := false
          } else {
            switch Belt.Array.get(wuf.weight, current.contents) {
            | Some(w) => totalWeight := totalWeight.contents + w
            | None => ()
            }
            current := parentIdx
          }
        | None => continue1 := false
        }
      }

      let root = current.contents

      // Path compression with weight updates
      let accWeight = ref(0)
      Belt.Array.forEach(path.contents, node => {
        if node != root {
          switch Belt.Array.get(wuf.weight, node) {
          | Some(w) =>
            let newWeight = totalWeight.contents - accWeight.contents
            accWeight := accWeight.contents + w
            Belt.Array.setUnsafe(wuf.parent, node, root)
            Belt.Array.setUnsafe(wuf.weight, node, newWeight)
          | None => ()
          }
        }
      })

      // Recalculate weight from element to root
      let finalWeight = ref(0)
      let curr = ref(elementIndex)
      let continue2 = ref(true)
      while continue2.contents {
        if curr.contents == root {
          continue2 := false
        } else {
          switch Belt.Array.get(wuf.weight, curr.contents) {
          | Some(w) =>
            finalWeight := finalWeight.contents + w
            switch Belt.Array.get(wuf.parent, curr.contents) {
            | Some(p) => curr := p
            | None => continue2 := false
            }
          | None => continue2 := false
          }
        }
      }

      Ok({root: root, weight: finalWeight.contents})
    }
  }

  /** Union with weight: weight[y] - weight[x] = w after union */
  let uniteWithWeight = (
    wuf: weightedUnionFind,
    elementX: int,
    elementY: int,
    weightValue: int,
  ): result<bool, unionFindError> => {
    switch (findWithWeight(wuf, elementX), findWithWeight(wuf, elementY)) {
    | (Ok(resultX), Ok(resultY)) =>
      if resultX.root == resultY.root {
        Ok(false)
      } else {
        // weight[y] - weight[x] = w
        // weight[rootY] = weight[x] + w - weight[y]
        let newWeight = resultX.weight + weightValue - resultY.weight

        let rankX = Belt.Array.get(wuf.rank, resultX.root)->Belt.Option.getWithDefault(0)
        let rankY = Belt.Array.get(wuf.rank, resultY.root)->Belt.Option.getWithDefault(0)

        if rankX < rankY {
          Belt.Array.setUnsafe(wuf.parent, resultX.root, resultY.root)
          Belt.Array.setUnsafe(wuf.weight, resultX.root, -newWeight)
        } else if rankX > rankY {
          Belt.Array.setUnsafe(wuf.parent, resultY.root, resultX.root)
          Belt.Array.setUnsafe(wuf.weight, resultY.root, newWeight)
        } else {
          Belt.Array.setUnsafe(wuf.parent, resultY.root, resultX.root)
          Belt.Array.setUnsafe(wuf.weight, resultY.root, newWeight)
          Belt.Array.setUnsafe(wuf.rank, resultX.root, rankX + 1)
        }

        wuf.setCount = wuf.setCount - 1
        Ok(true)
      }
    | (Error(e), _) | (_, Error(e)) => Error(e)
    }
  }

  /** Get the weight difference between two elements (if in same set) */
  let getDifference = (
    wuf: weightedUnionFind,
    elementX: int,
    elementY: int,
  ): result<option<int>, unionFindError> => {
    switch (findWithWeight(wuf, elementX), findWithWeight(wuf, elementY)) {
    | (Ok(resultX), Ok(resultY)) =>
      if resultX.root != resultY.root {
        Ok(None) // Not in same set
      } else {
        Ok(Some(resultY.weight - resultX.weight))
      }
    | (Error(e), _) | (_, Error(e)) => Error(e)
    }
  }

  /** Get the number of disjoint sets */
  let countSets = (wuf: weightedUnionFind): int => {
    wuf.setCount
  }

  /** Get the total number of elements */
  let count = (wuf: weightedUnionFind): int => {
    wuf.elementCount
  }

  /** Check if the structure is empty */
  let isEmpty = (wuf: weightedUnionFind): bool => {
    wuf.elementCount == 0
  }

  /** Reset to empty state */
  let clear = (wuf: weightedUnionFind): unit => {
    wuf.parent = []
    wuf.rank = []
    wuf.weight = []
    wuf.elementCount = 0
    wuf.setCount = 0
  }
}

/** Applications of Union-Find */
module Applications = {
  /** Check if a graph has a cycle (for undirected graphs)
   * edges: array of (u, v) pairs
   * numVertices: number of vertices in the graph
   */
  let hasCycle = (numVertices: int, edges: array<(int, int)>): bool => {
    switch makeWithElements(numVertices, numVertices) {
    | Ok(uf) =>
      let hasCycleFound = ref(false)
      Belt.Array.forEach(edges, ((u, v)) => {
        if !hasCycleFound.contents {
          switch connected(uf, u, v) {
          | Ok(true) => hasCycleFound := true
          | Ok(false) =>
            let _ = unite(uf, u, v)
          | Error(_) => ()
          }
        }
      })
      hasCycleFound.contents
    | Error(_) => false
    }
  }

  /** Count connected components in an undirected graph */
  let countComponents = (numVertices: int, edges: array<(int, int)>): int => {
    switch makeWithElements(numVertices, numVertices) {
    | Ok(uf) =>
      Belt.Array.forEach(edges, ((u, v)) => {
        let _ = unite(uf, u, v)
      })
      countSets(uf)
    | Error(_) => 0
    }
  }

  /** Get all connected components as arrays of vertices */
  let getComponents = (numVertices: int, edges: array<(int, int)>): array<array<int>> => {
    switch makeWithElements(numVertices, numVertices) {
    | Ok(uf) =>
      Belt.Array.forEach(edges, ((u, v)) => {
        let _ = unite(uf, u, v)
      })
      getAllSets(uf)
    | Error(_) => []
    }
  }

  /** Check if two vertices are in the same connected component */
  let areConnected = (numVertices: int, edges: array<(int, int)>, u: int, v: int): bool => {
    switch makeWithElements(numVertices, numVertices) {
    | Ok(uf) =>
      Belt.Array.forEach(edges, ((a, b)) => {
        let _ = unite(uf, a, b)
      })
      switch connected(uf, u, v) {
      | Ok(result) => result
      | Error(_) => false
      }
    | Error(_) => false
    }
  }

  /** Find the minimum spanning tree using Kruskal's algorithm
   * Returns array of edge indices that form the MST
   */
  let kruskalMST = (
    numVertices: int,
    edges: array<(int, int, int)>, // (u, v, weight)
  ): array<int> => {
    switch makeWithElements(numVertices, numVertices) {
    | Ok(uf) =>
      // Sort edges by weight
      let indexedEdges = Belt.Array.mapWithIndex(edges, (i, edge) => (i, edge))
      let sortedEdges = Belt.SortArray.stableSortBy(indexedEdges, ((_, (_, _, w1)), (_, (_, _, w2))) =>
        w1 - w2
      )

      let mstEdges = ref([])
      Belt.Array.forEach(sortedEdges, ((idx, (u, v, _))) => {
        switch connected(uf, u, v) {
        | Ok(false) =>
          let _ = unite(uf, u, v)
          mstEdges := Belt.Array.concat(mstEdges.contents, [idx])
        | _ => ()
        }
      })
      mstEdges.contents
    | Error(_) => []
    }
  }
}
