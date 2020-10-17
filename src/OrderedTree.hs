module OrderedTree
  ( OrderedTree,
    insert,
    inorder,
    fromList,


  )
where

import Tree (Tree (..))
import qualified Tree as T (inorder)

newtype OrderedTree a = OrderedTree (Tree a) deriving (Eq)

instance Show a => Show (OrderedTree a) where
  show (OrderedTree t) = show t

instance Ord a => Semigroup (OrderedTree a) where
  t1 <> (OrderedTree t2) = foldr insert t1 (T.inorder t2)

instance Ord a => Monoid (OrderedTree a) where
  mempty = OrderedTree Leaf

-- I have a binary tree whose node are sorted: i.e., each node in the left branch is smaller than the root, each node in the right branch is larger
-- I want to:
--   - implement an insert function that preserves this ordering property
--     insert :: (Ord a) => a -> OrderedTree a -> OrderedTree a
--   - implement a smart constructor from lists to ordered trees
--     fromList :: (Ord a) => [a] -> OrderedTree a
--   - re-implement tree traversal
--     inorder :: OrderedTree a -> [a]
--   - a QuickCheck property that checks if the inorder traversal of a tree built with "insert" is actually ordered
--   - (optional) a smart constructor that enforces the ordered property

insert :: (Ord a) => a -> OrderedTree a -> OrderedTree a
insert item (OrderedTree t) = OrderedTree $ insert' item t
  where
    insert' x Leaf = Node Leaf x Leaf
    insert' x (Node l c r)
      | x <= c = Node (insert' x l) c r
      | otherwise = Node l c (insert' x r)

fromList :: (Ord a) => [a] -> OrderedTree a
fromList l = foldl (flip insert) mempty l

inorder :: OrderedTree a -> [a]
inorder (OrderedTree t) = T.inorder t
