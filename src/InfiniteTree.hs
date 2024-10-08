{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}

module InfiniteTree
  ( infiniteTupleTree,
    cut,
  )
where

import Tree (Tree (..), Order(..), SOrder(..))

-- I have a binary tree whose nodes are (Integer, Integer) tuples that tell me how many times I brnched left or right
-- ex. Root is (0,0), left branch has node (1,0), right branch has node (0,1)
-- I want to implement:
--   - the infinite data structure
--     infiniteTupleTree :: Tree (Integer, Integer)
--   - the cutting function that "cuts" an infinite tree and produces a finite tree of the specified depth
--     cut :: Integer -> Tree a -> Tree a

newtype InfiniteTree a = Infinite (Tree LevelOrder a) deriving (Functor)

infiniteTupleTree :: InfiniteTree (Integer, Integer)
infiniteTupleTree = Infinite $ make_tree (0, 0)
  where
    make_tree (l, r) = Node SLevelOrder (l, r) left_subtree right_subtree
      where
        left_subtree = make_tree (l + 1, r)
        right_subtree = make_tree (l, r + 1)

newtype FiniteTree o a = Finite (Tree o a) deriving (Eq, Functor)

cut :: Integer -> InfiniteTree a -> FiniteTree LevelOrder a
cut depth (Infinite t) = Finite $ cut' depth t
  where
    cut' 0 _ = Leaf
    cut' _ Leaf = Leaf
    cut' d (Node o x l r) = Node o x left_cut right_cut
      where
        left_cut = cut' (d - 1) l
        right_cut = cut' (d - 1) r

instance Show (FiniteTree o (Integer, Integer)) where
  show (Finite t) = show . fmap represent $ t
    where
      represent (numL, numR) = "-" <> replicate (fromInteger numL) 'L' <> replicate (fromInteger numR) 'R'
