{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}

module Tree where

import Types (Iso (..), IsoOrd (..))

data Tree a = Leaf | Node a (Tree a) (Tree a) deriving (Eq, Functor)

-- PRINTING
      show' _ Leaf = ""
      show' depth (Node x l r) = spacing ++ show x ++ "\n" ++ show' (depth + 2) l ++ show' (depth + 2) r
        where
          spacing = take depth $ repeat ' '

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
showStructure = show . fmap (const "X")

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
