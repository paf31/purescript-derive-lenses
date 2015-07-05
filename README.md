# purescript-derive-lenses

A little utility to derive lenses and prisms for data types in PureScript

## Usage

Provide a PureScript module on standard input, and a new module will be returned on standard output

## How it Works

This tool generates the following types of optics:

- Prisms for data constructors which have zero or one argument
- Lenses for object fields appearing in data constructors

Instead of importing `purescript-lens`, the generated module directly uses the underlying modules (`Data.Profunctor` etc.) to keep the dependency set minimal. However, the generated lenses are compatible with the `purescript-lens` set of libraries.

## Example

Input file:

```purescript
module Data.Tree where

data Tree a
  = Leaf 
  | Branch { l :: Tree a, value :: a, r :: Tree a }
```

Output file:

```purescript
module Data.Tree.Lenses where

import Prelude

import Data.Either (Either(..))
import Data.Profunctor (dimap)
import Data.Profunctor.Choice (Choice, right)

import Data.Tree

l :: forall r a f. (Functor f) => (a -> f a) -> { left :: a | r } -> f { left :: a | r }
l f o = map(o{ left = _ })(f(o.left))

value :: forall r a f. (Functor f) => (a -> f a) -> { value :: a | r } -> f { value :: a | r }
value f o = map(o{ value = _ })(f(o.value))

r :: forall r a f. (Functor f) => (a -> f a) -> { right :: a | r } -> f { right :: a | r }
r f o = map(o{ right = _ })(f(o.right))

_Leaf :: forall a p f. (Applicative f, Choice p) => p Unit (f Unit) -> p (Tree a) (f (Tree a))
_Leaf p = dimap unwrap (pure <<< rewrap) (right p)
  where
  unwrap Leaf = Right unit
  unwrap y = Left y
  rewrap (Left y) = y
  rewrap (Right _) = Leaf

_Branch :: forall a p f. (Applicative f, Choice p) => p { right :: Tree a, value :: a, left :: Tree a } (f { right :: Tree a, value :: a, left :: Tree a }) -> p (Tree a) (f (Tree a))
_Branch p = dimap unwrap rewrap (right p)
  where
  unwrap (Branch x) = Right x
  unwrap y = Left y
  rewrap (Left y) = pure y
  rewrap (Right x) = map Branch x
```

These optics can now be composed in the usual ways:

```purescript
leftRight :: forall a f. (Applicative f) => (Tree a -> f (Tree a)) -> Tree a -> f (Tree a)
leftRight = r <<< _Branch <<< l <<< _Branch
```
