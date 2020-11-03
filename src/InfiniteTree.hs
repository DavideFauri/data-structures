module InfiniteTree
  ( inv_tup_tree,
    cut,
  )
where

import Tree (Tree (..))

-- I have a binary tree whose nodes are (Integer, Integer) tuples that tell me how many times I brnched left or right
-- ex. Root is (0,0), left branch has node (1,0), right branch has node (0,1)
-- I want to implement:
--   - the infinite data structure
--     inv_tup_tree :: Tree (Integer, Integer)
--   - the cutting function that "cuts" an infinite tree and produces a finite tree of the specified depth
--     cut :: Integer -> Tree a -> Tree a

inv_tup_tree :: Tree (Integer, Integer)
inv_tup_tree = make_tree (0, 0)
  where
    make_tree (l, r) = Node (l, r) left_subtree right_subtree
      where
        left_subtree = make_tree (l + 1, r)
        right_subtree = make_tree (l, r + 1)

cut :: Integer -> Tree a -> Tree a
cut 0 _ = Leaf
cut _ Leaf = Leaf
cut depth (Node x l r) = Node x left_cut right_cut
  where
    left_cut = (cut (depth -1) l)
    right_cut = (cut (depth -1) r)
