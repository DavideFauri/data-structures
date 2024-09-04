{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module OrderedTree
  ( OrderedTree,
    insert,
    fromList,
    toList,
  )
where

import Data.Foldable (toList)
import Tree (SOrder (..), Tree (..))

newtype OrderedTree o a = OrderedTree (Tree o a) deriving (Eq, Show, Functor, Foldable, Traversable)

instance (Ord a) => Semigroup (OrderedTree o a) where
  (<>) = foldr insert

instance (Ord a) => Monoid (OrderedTree o a) where
  mempty = OrderedTree Leaf

-- I have a binary tree whose node are sorted: i.e., each node in the left branch is smaller than the root, each node in the right branch is larger
-- I want to:
--   - implement an insert function that preserves this ordering property
--     insert :: (Ord a) => a -> OrderedTree a -> OrderedTree a
--   - implement a smart constructor from lists to ordered trees
--     fromList :: (Ord a) => [a] -> OrderedTree a
--   - a QuickCheck property that checks if the inorder traversal of a tree built with "insert" is actually ordered
--   - (optional) a smart constructor that enforces the ordered property

insert :: (Ord a) => a -> OrderedTree o a -> OrderedTree o a
insert item (OrderedTree t) = OrderedTree $ insert' item t
  where
    insert' x Leaf = Node _ x Leaf Leaf
    insert' x (Node o c l r)
      | x <= c = Node o c (insert' x l) r
      | otherwise = Node o c l (insert' x r)

insert' :: (Ord a) => a -> OrderedTree o a -> OrderedTree o a
insert' item (OrderedTree root) = OrderedTree $ insert'' item root
  where
    insert'' :: (Ord a) => a -> Tree o a -> Tree o a
    insert'' x Leaf = Node _ x Leaf Leaf
    insert'' x (Node SInOrder c l r)
      | x <= c = Node SInOrder c (insert'' x l) r
      | otherwise = Node SInOrder c l (insert'' x r)
    insert'' x (Node SPreOrder c l r)
      | x <= c = Node SPreOrder _ _ _
      | otherwise = Node SPreOrder _ _ _
    insert'' x (Node SPostOrder c l r)
      | x <= c = Node SPostOrder _ _ _
      | otherwise = Node SPostOrder _ _ _
    insert'' x (Node SLevelOrder _ _ _) = _

fromList :: (Ord a) => [a] -> OrderedTree o a
fromList = foldl (flip insert) mempty
