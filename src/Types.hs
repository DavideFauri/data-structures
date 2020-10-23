module Types where

-- This typeclass is a variant of Eq:
-- It is used to test if two structures are isomorphic, with no regard to the values contained within
-- Ex. [1,2,3] and "foo" are isomorphic because they are lists with 3 elements
class Iso a where
  (=~=), (/~=) :: a -> a -> Bool
  a /~= b = not (a =~= b)
