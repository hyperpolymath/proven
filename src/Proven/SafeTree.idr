-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>
||| SafeTree - Safe tree data structure operations
|||
||| This module provides various tree structures with safe
||| traversal and manipulation operations.
module Proven.SafeTree
import Data.String
import Data.List

import public Proven.Core

%default total

--------------------------------------------------------------------------------
-- Binary Tree
--------------------------------------------------------------------------------

||| Binary tree data structure
public export
data BinaryTree : Type -> Type where
  Leaf : BinaryTree a
  Node : (value : a) -> (left : BinaryTree a) -> (right : BinaryTree a) -> BinaryTree a

||| Create an empty tree
public export
emptyTree : BinaryTree a
emptyTree = Leaf

||| Create a single-node tree
public export
singleton : a -> BinaryTree a
singleton x = Node x Leaf Leaf

||| Check if tree is empty
public export
isEmptyTree : BinaryTree a -> Bool
isEmptyTree Leaf = True
isEmptyTree _ = False

||| Get the root value
public export
root : BinaryTree a -> Maybe a
root Leaf = Nothing
root (Node v _ _) = Just v

||| Get left subtree
public export
leftChild : BinaryTree a -> BinaryTree a
leftChild Leaf = Leaf
leftChild (Node _ l _) = l

||| Get right subtree
public export
rightChild : BinaryTree a -> BinaryTree a
rightChild Leaf = Leaf
rightChild (Node _ _ r) = r

--------------------------------------------------------------------------------
-- Tree Metrics
--------------------------------------------------------------------------------

||| Count nodes in tree
public export
size : BinaryTree a -> Nat
size Leaf = 0
size (Node _ l r) = S (size l + size r)

||| Calculate tree height
public export
height : BinaryTree a -> Nat
height Leaf = 0
height (Node _ l r) = S (max (height l) (height r))

||| Count leaf nodes
public export
leafCount : BinaryTree a -> Nat
leafCount Leaf = 1
leafCount (Node _ l r) = leafCount l + leafCount r

||| Check if tree is balanced (heights differ by at most 1)
public export
isBalanced : BinaryTree a -> Bool
isBalanced Leaf = True
isBalanced (Node _ l r) =
  let lh = height l
      rh = height r
      diff = if lh >= rh then minus lh rh else minus rh lh
  in diff <= 1 && isBalanced l && isBalanced r

--------------------------------------------------------------------------------
-- Traversals
--------------------------------------------------------------------------------

||| Pre-order traversal (root, left, right)
public export
preorder : BinaryTree a -> List a
preorder Leaf = []
preorder (Node v l r) = v :: preorder l ++ preorder r

||| In-order traversal (left, root, right)
public export
inorder : BinaryTree a -> List a
inorder Leaf = []
inorder (Node v l r) = inorder l ++ [v] ++ inorder r

||| Post-order traversal (left, right, root)
public export
postorder : BinaryTree a -> List a
postorder Leaf = []
postorder (Node v l r) = postorder l ++ postorder r ++ [v]

||| Level-order (breadth-first) traversal
public export
levelorder : BinaryTree a -> List a
levelorder tree = bfs [tree]
  where
    bfs : List (BinaryTree a) -> List a
    bfs [] = []
    bfs (Leaf :: rest) = bfs rest
    bfs (Node v l r :: rest) = v :: bfs (rest ++ [l, r])

--------------------------------------------------------------------------------
-- Binary Search Tree Operations
--------------------------------------------------------------------------------

||| Insert into BST
public export
bstInsert : Ord a => a -> BinaryTree a -> BinaryTree a
bstInsert x Leaf = singleton x
bstInsert x (Node v l r) =
  case compare x v of
    LT => Node v (bstInsert x l) r
    EQ => Node v l r  -- No duplicates
    GT => Node v l (bstInsert x r)

||| Search in BST
public export
bstContains : Ord a => a -> BinaryTree a -> Bool
bstContains _ Leaf = False
bstContains x (Node v l r) =
  case compare x v of
    LT => bstContains x l
    EQ => True
    GT => bstContains x r

||| Find minimum in BST
public export
bstMin : BinaryTree a -> Maybe a
bstMin Leaf = Nothing
bstMin (Node v Leaf _) = Just v
bstMin (Node _ l _) = bstMin l

||| Find maximum in BST
public export
bstMax : BinaryTree a -> Maybe a
bstMax Leaf = Nothing
bstMax (Node v _ Leaf) = Just v
bstMax (Node _ _ r) = bstMax r

||| Check if tree is a valid BST
public export
isBST : Ord a => BinaryTree a -> Bool
isBST tree = isSorted (inorder tree)
  where
    isSorted : Ord b => List b -> Bool
    isSorted [] = True
    isSorted [_] = True
    isSorted (x :: y :: xs) = x <= y && isSorted (y :: xs)

||| Build BST from list
public export
fromList : Ord a => List a -> BinaryTree a
fromList = foldl (flip bstInsert) Leaf

--------------------------------------------------------------------------------
-- Tree Transformations
--------------------------------------------------------------------------------

||| Map a function over tree values
public export
mapTree : (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node v l r) = Node (f v) (mapTree f l) (mapTree f r)

||| Fold tree (pre-order)
public export
foldTree : (a -> b -> b -> b) -> b -> BinaryTree a -> b
foldTree _ z Leaf = z
foldTree f z (Node v l r) = f v (foldTree f z l) (foldTree f z r)

||| Filter tree (keeps structure, replaces filtered nodes with Leaf)
public export
filterTree : (a -> Bool) -> BinaryTree a -> BinaryTree a
filterTree _ Leaf = Leaf
filterTree p (Node v l r) =
  if p v
    then Node v (filterTree p l) (filterTree p r)
    else Leaf  -- Remove node and its subtrees

||| Mirror a tree (swap left and right children)
public export
mirror : BinaryTree a -> BinaryTree a
mirror Leaf = Leaf
mirror (Node v l r) = Node v (mirror r) (mirror l)

--------------------------------------------------------------------------------
-- N-ary Tree
--------------------------------------------------------------------------------

||| N-ary tree (rose tree)
public export
data NTree : Type -> Type where
  NNode : (value : a) -> (children : List (NTree a)) -> NTree a

||| Create a leaf node (no children)
public export
nleaf : a -> NTree a
nleaf x = NNode x []

||| Get value from n-ary tree node
public export
nvalue : NTree a -> a
nvalue (NNode v _) = v

||| Get children of n-ary tree node
public export
nchildren : NTree a -> List (NTree a)
nchildren (NNode _ cs) = cs

||| Size of n-ary tree
public export
nsize : NTree a -> Nat
nsize (NNode _ cs) = S (sum (map nsize cs))
  where
    sum : List Nat -> Nat
    sum [] = 0
    sum (x :: xs) = x + sum xs

||| Flatten n-ary tree to list (pre-order)
public export
nflatten : NTree a -> List a
nflatten (NNode v cs) = v :: concatMap nflatten cs

--------------------------------------------------------------------------------
-- Display
--------------------------------------------------------------------------------

public export
Show a => Show (BinaryTree a) where
  show Leaf = "Leaf"
  show (Node v l r) = "Node(" ++ show v ++ ")"

public export
Show a => Show (NTree a) where
  show (NNode v cs) = "NTree(" ++ show v ++ ", " ++ show (length cs) ++ " children)"

