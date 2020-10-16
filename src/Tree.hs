{-# LANGUAGE FlexibleInstances #-}

module Tree where

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving (Eq)

instance Show a => Show (Tree a) where
    show t = show' 0 t where
        show' _ Leaf = ""
        show' depth (Node l c r) = "\n" ++ spacing ++ show c ++ show' (depth+2) l ++ show' (depth+2) r
            where
                spacing = take depth $ repeat ' '

inorder :: Tree a -> [a]
inorder Leaf = []
inorder (Node l c r) = inorder l <> [c] <> inorder r
