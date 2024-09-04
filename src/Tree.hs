{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
-- for instance Show (Tree String)
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
-- for instance Show on overlapping types (String and Show a)
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}

module Tree where

import Types (Iso (..), IsoOrd (..))

data Order
  = NoOrder
  | InOrder
  | PreOrder
  | PostOrder
  | LevelOrder
  deriving (Eq, Show)

-- singleton type, for pattern matching
data SOrder (o :: Order) where
  -- SNoOrder :: SOrder NoOrder
  SInOrder :: SOrder InOrder
  SPreOrder :: SOrder PreOrder
  SPostOrder :: SOrder PostOrder
  SLevelOrder :: SOrder LevelOrder

deriving instance Eq (SOrder o)

deriving instance Show (SOrder o)

data Tree (o :: Order) a = Leaf | Node (SOrder o) a (Tree o a) (Tree o a) deriving (Eq, Functor)

-- PRINTING

_prettyPrintWith :: (a -> String) -> Tree o a -> String
_prettyPrintWith customShow = prettyPrint 0
  where
    prettyPrint _ Leaf = ""
    prettyPrint depth (Node _ x l r) =
      spacing
        <> customShow x
        <> "\n"
        <> prettyPrint (depth + 2) l
        <> prettyPrint (depth + 2) r
      where
        spacing = replicate depth ' '

instance Show (Tree o String) where
  show :: Tree o String -> String
  show = _prettyPrintWith id

instance (Show a) => Show (Tree o a) where
  show :: Tree o a -> String
  show = _prettyPrintWith show

-- STRUCTURE

instance Iso (Tree o) where
  (~==) :: Tree o a -> Tree o b -> Bool
  Leaf ~== Leaf = True
  Leaf ~== Node {} = False
  Node {} ~== Leaf = False
  -- We DO NOT consider flipping L/R branches when checking for isomorphism between trees
  -- This is because traversal is important, and we don't want to mess it up
  Node _ _ l1 r1 ~== Node _ _ l2 r2 = (l1 ~== l2) && (r1 ~== r2)

instance IsoOrd (Tree o) where
  compare :: Tree o a -> Tree o b -> Maybe Ordering
  compare Leaf Leaf = Just EQ
  compare Leaf (Node {}) = Just LT
  compare (Node {}) Leaf = Just GT
  compare (Node _ _ l1 r1) (Node _ _ l2 r2)
    | l1 ~== l2 && r1 ~== r2 = Just EQ
    | l1 ~<= l2 && r1 ~<= r2 = Just LT
    | l1 ~>= l2 && r1 ~>= r2 = Just GT
    | otherwise = Nothing

showStructure :: Tree o a -> String
showStructure = show . remap "X"
  where
    remap _ Leaf = Leaf
    remap c (Node o _ l r) = Node o c (remap "L" l) (remap "R" r)

-- DEPTH FIRST TRAVERSALS

instance Foldable (Tree o) where
  foldMap _ Leaf = mempty
  foldMap f (Node SInOrder x l r) = foldMap f l <> f x <> foldMap f r
  foldMap f (Node SPreOrder x l r) = f x <> foldMap f l <> foldMap f r
  foldMap f (Node SPostOrder x l r) = foldMap f l <> foldMap f r <> f x
-- thanks to https://www.jjinux.com/2005/12/haskell-breadth-first-tree-traversal.html
  foldMap f root@(Node SLevelOrder _ _ _) = foldMap funcOnValue $ breadthFirst [root]
    where
      funcOnValue (Node _ x _ _) = f x
      funcOnValue Leaf = mempty

      breadthFirst [] = []
      breadthFirst nodes@(_ : _) = nodes <> breadthFirst (concatMap getChildren nodes)

      getChildren (Node _ _ l r) = [l, r]
      getChildren Leaf = []

instance Traversable (Tree o) where
  sequenceA Leaf = pure Leaf
  sequenceA (Node SInOrder x l r) = do
    l' <- sequenceA l
    x' <- x
    r' <- sequenceA r
    return $ Node SInOrder x' l' r'
  sequenceA (Node SPreOrder x l r) = do
    x' <- x
    l' <- sequenceA l
    r' <- sequenceA r
    return $ Node SPreOrder x' l' r'
  sequenceA (Node SPostOrder x l r) = do
    l' <- sequenceA l
    r' <- sequenceA r
    x' <- x
    return $ Node SPostOrder x' l' r'
  sequenceA (Node SLevelOrder _ _ _) = pure Leaf -- TODO
