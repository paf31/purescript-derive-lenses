module Data.Tree where

data Tree a
  = Leaf
  | Branch { l :: Tree a, value :: a, r :: Tree a }
