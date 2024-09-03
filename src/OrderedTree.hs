{-# LANGUAGE DeriveFoldable #-}

module OrderedTree
  ( OrderedTree,
    insert,
    fromList,
    toList,
  )
where

import Data.Foldable (toList)
import Tree (InOrder (..), Tree (..))

newtype OrderedTree a = OrderedTree (InOrder a) deriving (Eq, Show, Foldable)

instance (Ord a) => Semigroup (OrderedTree a) where
  (<>) = foldr insert

instance (Ord a) => Monoid (OrderedTree a) where
  mempty = OrderedTree . InOrder $ Leaf

-- I have a binary tree whose node are sorted: i.e., each node in the left branch is smaller than the root, each node in the right branch is larger
-- I want to:
--   - implement an insert function that preserves this ordering property
--     insert :: (Ord a) => a -> OrderedTree a -> OrderedTree a
--   - implement a smart constructor from lists to ordered trees
--     fromList :: (Ord a) => [a] -> OrderedTree a
--   - a QuickCheck property that checks if the inorder traversal of a tree built with "insert" is actually ordered
--   - (optional) a smart constructor that enforces the ordered property

insert :: (Ord a) => a -> OrderedTree a -> OrderedTree a
insert item (OrderedTree (InOrder t)) = OrderedTree . InOrder $ insert' item t
  where
    insert' x Leaf = Node x Leaf Leaf
    insert' x (Node c l r)
      | x <= c = Node c (insert' x l) r
      | otherwise = Node c l (insert' x r)

fromList :: (Ord a) => [a] -> OrderedTree a
fromList = foldl (flip insert) mempty
