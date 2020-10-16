module OrderedTree ( OrderedTree
                   , insert
                   , fromList
                   ) where



import Tree (Tree(..), inorder)

newtype OrderedTree a = OrderedTree (Tree a)

instance Ord a => Semigroup (OrderedTree a) where
  (<>) t1 (OrderedTree t2) = foldr insert t1 (inorder t2)

instance Ord a => Monoid (OrderedTree a) where
  mempty = OrderedTree Leaf

-- I have a binary tree whose node are sorted: i.e., each node in the left branch is smaller than the root, each node in the right branch is larger
-- I want to implement:
--   - an insert function that
--     insert :: (Ord a) => a -> Tree a -> Tree a
--   - the cutting function that "cuts" an infinite tree and produces a finite tree of the specified depth
--     inorder :: Tree a -> [a]
--   - a QuickCheck property that checks if the inorder traversal of a tree built with "insert" is actually ordered
--   - (optional) a smart constructor that enforces the ordered property

insert :: (Ord a) => a -> OrderedTree a -> OrderedTree a
insert item (OrderedTree t) = OrderedTree $ insert' item t
  where
    insert' x Leaf = Node Leaf x Leaf
    insert' x (Node l c r)
      | x <= c = Node (insert' x l) c r
      | otherwise = Node l c (insert' x r)

-- SMART CONSTRUCTOR

fromList :: Ord a => [a] -> OrderedTree a
fromList l = foldl (flip insert) (OrderedTree Leaf) l
