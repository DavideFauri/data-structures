module Types where

-- This typeclass is a variant of Eq:
-- It is used to test if two structures are isomorphic, with no regard to the values contained within
-- Ex. [1,2,3] and "foo" are isomorphic because they are lists with 3 elements
class Iso m where
  (~==), (~/=) :: m a -> m b -> Bool
  x ~/= y = not (x ~== y)

  {-# MINIMAL (~==) #-}

-- This typeclass is a variant of Ord:
-- It is used to test if two structures satisfy the subgraph isomorphism problem
-- That is, if the subset of a structure is isomorphic to the other one
class Iso m => IsoOrd m where
  -- first structure is isomorphic to (subset of) second structure
  (~<=) :: m a -> m b -> Bool
  s ~<= s' = case Types.compare s s' of
    Just EQ -> True
    Just LT -> True
    _ -> False

  -- second structure is isomorphic to (subset of) first structure
  (~>=) :: m a -> m b -> Bool
  s ~>= s' = s' ~<= s

  -- first structure is isomorphic to strict subset of second structure
  (~<) :: m a -> m b -> Bool
  s ~< s' = s ~<= s' && s ~/= s'

  -- second structure is isomorphic to strict subset of first structure
  (~>) :: m a -> m b -> Bool
  s ~> s' = s ~>= s' && s ~/= s'

  compare :: m a -> m b -> Maybe Ordering
  compare s s'
    | s ~== s' = Just EQ
    | s ~< s' = Just LT
    | s ~> s' = Just GT
    | otherwise = Nothing

  {-# MINIMAL (~<=) | compare #-}
