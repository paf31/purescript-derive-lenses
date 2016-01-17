module Data.Tree where

type Foo = {foo :: Int}

data Tree a
  = Leaf
  | Branch { l :: Tree a, value :: a, r :: Tree a }
