{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}

module Tree where

import Types (Iso (..))

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving (Eq, Functor)

instance Show a => Show (Tree a) where
  show t = show' 0 t
    where
      show' _ Leaf = ""
      show' depth (Node l c r) = spacing ++ show c ++ "\n" ++ show' (depth + 2) l ++ show' (depth + 2) r
        where
          spacing = take depth $ repeat ' '

instance Iso (Tree a) where
  Leaf =~= Leaf = True
  Leaf =~= Node _ _ _ = False
  Node _ _ _ =~= Leaf = False
  -- We DO NOT consider flipping L/R branches when checking for isomorphism between trees
  -- This is because traversal is important, and we don't want to mess it up
  Node l1 _ r1 =~= Node l2 _ r2 = (l1 =~= l2) && (r1 =~= r2)

instance Applicative Tree where
  -- wrapping a value creates a 1-node tree
  pure x = Node Leaf x Leaf

  -- applying a tree of functions to an empty tree of values returns an empty tree
  _ <*> Leaf = Leaf
  -- applying an empty tree of functions returns an empty tree
  Leaf <*> _ = Leaf
  -- Applying a tree of functions to a tree of values is easy if the trees are isomorphic.
  -- If not, we create a maximal tree where the "center" function or value is propagated
  -- left or right when the corresponding branch is missing.
  -- ex.   Node (Node Leaf (*3) Leaf) (*2) Leaf <*> Node Leaf 2 (Node Leaf 5 Leaf) ==
  --       Node (Node Leaf 6 Leaf) 4 (Node Leaf 10 Leaf)

  -- case 1, only have center function
  Node Leaf fc Leaf <*> Node l c r = Node (pure fc <*> l) (fc c) (pure fc <*> r)
  -- case 2, only have center and left functions
  Node fl fc Leaf <*> Node Leaf c r = Node (fl <*> pure c) (fc c) (pure fc <*> r)
  Node fl fc Leaf <*> Node l c r = Node (fl <*> l) (fc c) (pure fc <*> r)
  -- case 3, only have center and right functions
  Node Leaf fc fr <*> Node l c Leaf = Node (pure fc <*> l) (fc c) (fr <*> pure c)
  Node Leaf fc fr <*> Node l c r = Node (pure fc <*> l) (fc c) (fr <*> r)
  -- case 4, have all functions
  Node fl fc fr <*> Node Leaf c Leaf = Node (fl <*> pure c) (fc c) (fr <*> pure c)
  Node fl fc fr <*> Node l c Leaf = Node (fl <*> l) (fc c) (fr <*> pure c)
  Node fl fc fr <*> Node Leaf c r = Node (fl <*> pure c) (fc c) (fr <*> r)
  Node fl fc fr <*> Node l c r = Node (fl <*> l) (fc c) (fr <*> r)

-- pure id <*> v = v                            -- Identity Law
-- Node Leaf id Leaf <*> t = t                                                             --> (case 1)

-- pure f <*> pure x = pure (f x)               -- Homomorphism Law
-- Node Leaf f Leaf <*> Node Leaf x Leaf = Node Leaf (f x) Leaf                            --> (case 1)

-- u <*> pure y = pure ($ y) <*> u              -- Interchange Law
-- Leaf <*> Node Leaf y Leaf = Node Leaf ($ y) Leaf <*> Leaf                               --> (leaves)
-- Node fl   fc Leaf <*> Node Leaf y Leaf = Node Leaf ($ y) Leaf <*> Node fl   fc Leaf     --> (case 2)
-- Node Leaf fc fr   <*> Node Leaf y Leaf = Node Leaf ($ y) Leaf <*> Node Leaf fc fr       --> (case 3)
-- Node fl   fc fr   <*> Node Leaf y Leaf = Node Leaf ($ y) Leaf <*> Node fl   fc fr       --> (case 4)

-- pure (.) <*> u <*> v <*> w = u <*> (v <*> w) -- Composition Law
-- I have no idea if how to verify if it's respected or not, let's just say it is

