// SPDX-License-Identifier: PMPL-1.0
// SPDX-FileCopyrightText: 2025 Hyperpolymath

/**
 * SafeTree - Tree data structure operations that cannot crash.
 *
 * Provides bounded tree operations with configurable maximum depth
 * and node count. Supports common tree traversals, path operations,
 * and structural queries while preventing unbounded recursion.
 */

/** Error types for tree operations */
type treeError =
  | MaxDepthExceeded
  | MaxNodesReached
  | NodeNotFound
  | InvalidParent
  | CycleDetected
  | RootAlreadyExists
  | CannotRemoveRoot

/** Default maximum tree depth */
let defaultMaxDepth = 64

/** Default maximum nodes per tree */
let defaultMaxNodes = 1024

/** Node identifier type */
type nodeId = int

/** Sentinel value for no parent (root node) */
let noParent: nodeId = -1

/** A tree node with value and parent reference */
type treeNode<'a> = {
  value: 'a,
  parent: nodeId,
  depth: int,
  mutable childCount: int,
}

/** A safe tree data structure with bounded depth and node count */
type t<'a> = {
  mutable nodes: array<option<treeNode<'a>>>,
  mutable nodeCount: int,
  mutable root: option<nodeId>,
  maxNodes: int,
  maxDepth: int,
}

/** Initialize a new empty tree with default limits */
let make = (): t<'a> => {
  {
    nodes: [],
    nodeCount: 0,
    root: None,
    maxNodes: defaultMaxNodes,
    maxDepth: defaultMaxDepth,
  }
}

/** Initialize a new empty tree with custom limits */
let makeWithLimits = (maxNodes: int, maxDepth: int): t<'a> => {
  {
    nodes: [],
    nodeCount: 0,
    root: None,
    maxNodes: maxNodes,
    maxDepth: maxDepth,
  }
}

/** Get the number of nodes in the tree */
let length = (tree: t<'a>): int => {
  tree.nodeCount
}

/** Check if tree is empty */
let isEmpty = (tree: t<'a>): bool => {
  tree.nodeCount == 0
}

/** Get the root node ID */
let getRoot = (tree: t<'a>): option<nodeId> => {
  tree.root
}

/** Set the root node (creates new tree or fails if root exists) */
let setRoot = (tree: t<'a>, value: 'a): result<nodeId, treeError> => {
  switch tree.root {
  | Some(_) => Error(RootAlreadyExists)
  | None =>
    if tree.nodeCount >= tree.maxNodes {
      Error(MaxNodesReached)
    } else {
      let nodeId = tree.nodeCount
      let node = {
        value: value,
        parent: noParent,
        depth: 0,
        childCount: 0,
      }
      tree.nodes = Belt.Array.concat(tree.nodes, [Some(node)])
      tree.nodeCount = tree.nodeCount + 1
      tree.root = Some(nodeId)
      Ok(nodeId)
    }
  }
}

/** Add a child node to a parent */
let addChild = (tree: t<'a>, parentId: nodeId, value: 'a): result<nodeId, treeError> => {
  if parentId < 0 || parentId >= tree.nodeCount {
    Error(InvalidParent)
  } else {
    switch Belt.Array.get(tree.nodes, parentId) {
    | None | Some(None) => Error(InvalidParent)
    | Some(Some(parentNode)) =>
      if tree.nodeCount >= tree.maxNodes {
        Error(MaxNodesReached)
      } else {
        let newDepth = parentNode.depth + 1
        if newDepth > tree.maxDepth {
          Error(MaxDepthExceeded)
        } else {
          let nodeId = tree.nodeCount
          let node = {
            value: value,
            parent: parentId,
            depth: newDepth,
            childCount: 0,
          }
          tree.nodes = Belt.Array.concat(tree.nodes, [Some(node)])
          tree.nodeCount = tree.nodeCount + 1
          // Update parent's child count
          parentNode.childCount = parentNode.childCount + 1
          Ok(nodeId)
        }
      }
    }
  }
}

/** Get a node's value */
let getValue = (tree: t<'a>, nodeId: nodeId): option<'a> => {
  if nodeId < 0 || nodeId >= tree.nodeCount {
    None
  } else {
    switch Belt.Array.get(tree.nodes, nodeId) {
    | None | Some(None) => None
    | Some(Some(node)) => Some(node.value)
    }
  }
}

/** Set a node's value */
let setValue = (tree: t<'a>, nodeId: nodeId, value: 'a): result<unit, treeError> => {
  if nodeId < 0 || nodeId >= tree.nodeCount {
    Error(NodeNotFound)
  } else {
    switch Belt.Array.get(tree.nodes, nodeId) {
    | None | Some(None) => Error(NodeNotFound)
    | Some(Some(node)) =>
      // Create a new node with updated value (mutate in place)
      let updatedNode = {...node, value: value}
      Belt.Array.setUnsafe(tree.nodes, nodeId, Some(updatedNode))
      Ok()
    }
  }
}

/** Get a node's parent ID */
let getParent = (tree: t<'a>, nodeId: nodeId): option<nodeId> => {
  if nodeId < 0 || nodeId >= tree.nodeCount {
    None
  } else {
    switch Belt.Array.get(tree.nodes, nodeId) {
    | None | Some(None) => None
    | Some(Some(node)) =>
      if node.parent == noParent {
        None
      } else {
        Some(node.parent)
      }
    }
  }
}

/** Get a node's depth */
let getDepth = (tree: t<'a>, nodeId: nodeId): option<int> => {
  if nodeId < 0 || nodeId >= tree.nodeCount {
    None
  } else {
    switch Belt.Array.get(tree.nodes, nodeId) {
    | None | Some(None) => None
    | Some(Some(node)) => Some(node.depth)
    }
  }
}

/** Check if node is the root */
let isRoot = (tree: t<'a>, nodeId: nodeId): bool => {
  switch tree.root {
  | Some(rootId) => nodeId == rootId
  | None => false
  }
}

/** Check if node is a leaf (no children) */
let isLeaf = (tree: t<'a>, nodeId: nodeId): bool => {
  if nodeId < 0 || nodeId >= tree.nodeCount {
    false
  } else {
    switch Belt.Array.get(tree.nodes, nodeId) {
    | None | Some(None) => false
    | Some(Some(node)) => node.childCount == 0
    }
  }
}

/** Get children of a node */
let getChildren = (tree: t<'a>, parentId: nodeId): array<nodeId> => {
  let children = ref([])
  for i in 0 to tree.nodeCount - 1 {
    switch Belt.Array.get(tree.nodes, i) {
    | Some(Some(node)) if node.parent == parentId =>
      children := Belt.Array.concat(children.contents, [i])
    | _ => ()
    }
  }
  children.contents
}

/** Get number of children for a node */
let getChildCount = (tree: t<'a>, nodeId: nodeId): int => {
  if nodeId < 0 || nodeId >= tree.nodeCount {
    0
  } else {
    switch Belt.Array.get(tree.nodes, nodeId) {
    | None | Some(None) => 0
    | Some(Some(node)) => node.childCount
    }
  }
}

/** Get path from root to node */
let getPath = (tree: t<'a>, nodeId: nodeId): array<nodeId> => {
  if nodeId < 0 || nodeId >= tree.nodeCount {
    []
  } else {
    switch Belt.Array.get(tree.nodes, nodeId) {
    | None | Some(None) => []
    | Some(Some(_)) =>
      let path = ref([])
      let current = ref(nodeId)
      let iterations = ref(0)
      let continue = ref(true)
      while continue.contents && iterations.contents <= tree.maxDepth {
        path := Belt.Array.concat([current.contents], path.contents)
        switch Belt.Array.get(tree.nodes, current.contents) {
        | Some(Some(node)) =>
          if node.parent == noParent {
            continue := false
          } else {
            current := node.parent
          }
        | _ => continue := false
        }
        iterations := iterations.contents + 1
      }
      path.contents
    }
  }
}

/** Check if node A is an ancestor of node B */
let isAncestor = (tree: t<'a>, ancestorId: nodeId, descendantId: nodeId): bool => {
  if ancestorId < 0 || ancestorId >= tree.nodeCount || descendantId < 0 || descendantId >= tree.nodeCount {
    false
  } else {
    let current = ref(descendantId)
    let found = ref(false)
    let iterations = ref(0)
    while !found.contents && iterations.contents <= tree.maxDepth {
      switch Belt.Array.get(tree.nodes, current.contents) {
      | Some(Some(node)) =>
        if node.parent == ancestorId {
          found := true
        } else if node.parent == noParent {
          iterations := tree.maxDepth + 1 // Exit loop
        } else {
          current := node.parent
        }
      | _ => iterations := tree.maxDepth + 1 // Exit loop
      }
      iterations := iterations.contents + 1
    }
    found.contents
  }
}

/** Get the lowest common ancestor of two nodes */
let lowestCommonAncestor = (tree: t<'a>, idA: nodeId, idB: nodeId): option<nodeId> => {
  if idA < 0 || idA >= tree.nodeCount || idB < 0 || idB >= tree.nodeCount {
    None
  } else {
    let pathA = getPath(tree, idA)
    let pathB = getPath(tree, idB)
    // Find the last common node in both paths
    let lca = ref(None)
    let minLen = Js.Math.min_int(Belt.Array.length(pathA), Belt.Array.length(pathB))
    for i in 0 to minLen - 1 {
      switch (Belt.Array.get(pathA, i), Belt.Array.get(pathB, i)) {
      | (Some(a), Some(b)) if a == b => lca := Some(a)
      | _ => ()
      }
    }
    lca.contents
  }
}

/** Get maximum depth in the tree */
let getMaxDepth = (tree: t<'a>): int => {
  let maxDepthVal = ref(0)
  for i in 0 to tree.nodeCount - 1 {
    switch Belt.Array.get(tree.nodes, i) {
    | Some(Some(node)) =>
      if node.depth > maxDepthVal.contents {
        maxDepthVal := node.depth
      }
    | _ => ()
    }
  }
  maxDepthVal.contents
}

/** Count leaf nodes */
let countLeaves = (tree: t<'a>): int => {
  let count = ref(0)
  for i in 0 to tree.nodeCount - 1 {
    switch Belt.Array.get(tree.nodes, i) {
    | Some(Some(node)) =>
      if node.childCount == 0 {
        count := count.contents + 1
      }
    | _ => ()
    }
  }
  count.contents
}

/** Perform pre-order traversal, returning node IDs */
let preorderTraversal = (tree: t<'a>): array<nodeId> => {
  switch tree.root {
  | None => []
  | Some(rootId) =>
    let result = ref([])
    let stack = ref([rootId])
    while Belt.Array.length(stack.contents) > 0 {
      let lastIdx = Belt.Array.length(stack.contents) - 1
      switch Belt.Array.get(stack.contents, lastIdx) {
      | Some(current) =>
        stack := Belt.Array.slice(stack.contents, ~offset=0, ~len=lastIdx)
        result := Belt.Array.concat(result.contents, [current])
        // Add children in reverse order
        let children = getChildren(tree, current)
        let reversedChildren = Belt.Array.reverse(children)
        stack := Belt.Array.concat(stack.contents, reversedChildren)
      | None => ()
      }
    }
    result.contents
  }
}

/** Perform level-order (breadth-first) traversal */
let levelOrderTraversal = (tree: t<'a>): array<nodeId> => {
  switch tree.root {
  | None => []
  | Some(rootId) =>
    let result = ref([])
    let queue = ref([rootId])
    while Belt.Array.length(queue.contents) > 0 {
      switch Belt.Array.get(queue.contents, 0) {
      | Some(current) =>
        queue := Belt.Array.sliceToEnd(queue.contents, 1)
        result := Belt.Array.concat(result.contents, [current])
        let children = getChildren(tree, current)
        queue := Belt.Array.concat(queue.contents, children)
      | None => ()
      }
    }
    result.contents
  }
}

/** Clear the tree */
let clear = (tree: t<'a>): unit => {
  tree.nodes = []
  tree.nodeCount = 0
  tree.root = None
}

/** Map a function over all node values */
let map = (tree: t<'a>, f: 'a => 'b): t<'b> => {
  let newTree = makeWithLimits(tree.maxNodes, tree.maxDepth)
  newTree.nodeCount = tree.nodeCount
  newTree.root = tree.root
  newTree.nodes = Belt.Array.map(tree.nodes, nodeOpt => {
    switch nodeOpt {
    | None => None
    | Some(node) =>
      Some({
        value: f(node.value),
        parent: node.parent,
        depth: node.depth,
        childCount: node.childCount,
      })
    }
  })
  newTree
}

/** Fold over all node values in pre-order */
let fold = (tree: t<'a>, init: 'b, f: ('b, 'a) => 'b): 'b => {
  let order = preorderTraversal(tree)
  Belt.Array.reduce(order, init, (acc, nodeId) => {
    switch getValue(tree, nodeId) {
    | Some(value) => f(acc, value)
    | None => acc
    }
  })
}

/** Find node IDs matching a predicate */
let filter = (tree: t<'a>, predicate: 'a => bool): array<nodeId> => {
  let result = ref([])
  for i in 0 to tree.nodeCount - 1 {
    switch Belt.Array.get(tree.nodes, i) {
    | Some(Some(node)) =>
      if predicate(node.value) {
        result := Belt.Array.concat(result.contents, [i])
      }
    | _ => ()
    }
  }
  result.contents
}

/** Find the first node matching a predicate */
let find = (tree: t<'a>, predicate: 'a => bool): option<nodeId> => {
  let result = ref(None)
  let i = ref(0)
  while result.contents == None && i.contents < tree.nodeCount {
    switch Belt.Array.get(tree.nodes, i.contents) {
    | Some(Some(node)) =>
      if predicate(node.value) {
        result := Some(i.contents)
      }
    | _ => ()
    }
    i := i.contents + 1
  }
  result.contents
}

/** Get all values at a specific depth */
let getValuesAtDepth = (tree: t<'a>, depth: int): array<'a> => {
  let result = ref([])
  for i in 0 to tree.nodeCount - 1 {
    switch Belt.Array.get(tree.nodes, i) {
    | Some(Some(node)) =>
      if node.depth == depth {
        result := Belt.Array.concat(result.contents, [node.value])
      }
    | _ => ()
    }
  }
  result.contents
}

/** Get the subtree rooted at a node (as array of node IDs) */
let getSubtree = (tree: t<'a>, rootId: nodeId): array<nodeId> => {
  if rootId < 0 || rootId >= tree.nodeCount {
    []
  } else {
    let result = ref([])
    let stack = ref([rootId])
    while Belt.Array.length(stack.contents) > 0 {
      let lastIdx = Belt.Array.length(stack.contents) - 1
      switch Belt.Array.get(stack.contents, lastIdx) {
      | Some(current) =>
        stack := Belt.Array.slice(stack.contents, ~offset=0, ~len=lastIdx)
        result := Belt.Array.concat(result.contents, [current])
        let children = getChildren(tree, current)
        stack := Belt.Array.concat(stack.contents, children)
      | None => ()
      }
    }
    result.contents
  }
}
