{-# LANGUAGE DeriveFunctor #-}
-- for instance Show (Tree String)
{-# LANGUAGE FlexibleInstances #-}
-- for instance Show on overlapping types (String and Show a)
{-# LANGUAGE IncoherentInstances #-}

module Tree where

import Types (Iso (..), IsoOrd (..))

data Tree a = Leaf | Node a (Tree a) (Tree a) deriving (Eq, Functor)

-- PRINTING

_prettyPrintWith :: (a -> String) -> Tree a -> String
_prettyPrintWith customShow tree = prettyPrint 0 tree
  where
    prettyPrint _ Leaf = ""
    prettyPrint depth (Node x l r) =
      spacing <> customShow x <> "\n"
        <> prettyPrint (depth + 2) l
        <> prettyPrint (depth + 2) r
      where
        spacing = take depth $ repeat ' '

instance Show (Tree String) where
  show = _prettyPrintWith id

instance Show a => Show (Tree a) where
  show = _prettyPrintWith show

-- STRUCTURE

instance Iso Tree where
  Leaf ~== Leaf = True
  Leaf ~== Node _ _ _ = False
  Node _ _ _ ~== Leaf = False
  -- We DO NOT consider flipping L/R branches when checking for isomorphism between trees
  -- This is because traversal is important, and we don't want to mess it up
  Node _ l1 r1 ~== Node _ l2 r2 = (l1 ~== l2) && (r1 ~== r2)

instance IsoOrd Tree where
  compare Leaf Leaf = Just EQ
  compare Leaf (Node _ _ _) = Just LT
  compare (Node _ _ _) Leaf = Just GT
  compare (Node _ l1 r1) (Node _ l2 r2)
    | l1 ~== l2 && r1 ~== r2 = Just EQ
    | l1 ~<= l2 && r1 ~<= r2 = Just LT
    | l1 ~>= l2 && r1 ~>= r2 = Just GT
    | otherwise = Nothing

showStructure :: Tree a -> String
showStructure = show . remap "X"
  where
    remap _ Leaf = Leaf
    remap c (Node _ l r) = Node c (remap "L" l) (remap "R" r)

-- DEPTH FIRST TRAVERSALS


-- INORDER

newtype InOrder a = InOrder (Tree a) deriving (Eq, Show, Functor)

instance Foldable InOrder where
  foldMap func (InOrder t) = foldMap' func t
    where
      foldMap' _ Leaf = mempty
      foldMap' f (Node x l r) = foldMap' f l <> f x <> foldMap' f r

-- PREORDER

newtype PreOrder a = PreOrder (Tree a) deriving (Eq, Show, Functor)

instance Foldable PreOrder where
  foldMap func (PreOrder t) = foldMap' func t
    where
      foldMap' _ Leaf = mempty
      foldMap' f (Node x l r) = f x <> foldMap' f l <> foldMap' f r

-- POSTORDER

newtype PostOrder a = PostOrder (Tree a) deriving (Eq, Show, Functor)

instance Foldable PostOrder where
  foldMap func (PostOrder t) = foldMap' func t
    where
      foldMap' _ Leaf = mempty
      foldMap' f (Node x l r) = foldMap' f l <> foldMap' f r <> f x


-- BREADTH FIRST TRAVERSAL (level order)

newtype LevelOrder a = LevelOrder (Tree a) deriving (Eq, Show, Functor)

-- thanks to https://www.jjinux.com/2005/12/haskell-breadth-first-tree-traversal.html
instance Foldable LevelOrder where
  foldMap func (LevelOrder root) = foldMap funcOnValue $ breadthFirst [root]
    where
      funcOnValue (Node x _ _) = func x
      funcOnValue Leaf = mempty

      breadthFirst [] = []
      breadthFirst nodes@(_ : _) = nodes <> (breadthFirst $ concatMap getChildren nodes)

      getChildren (Node _ l r) = [l, r]
      getChildren Leaf = []
