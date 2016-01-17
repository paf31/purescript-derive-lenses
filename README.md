# purescript-derive-lenses

A little utility to derive lenses and prisms for data types in PureScript

## Usage

Provide a PureScript module on standard input, and a new module will be returned on standard output

## How it Works

This tool generates the following types of optics:

- Prisms for data constructors which have zero or one argument
- Lenses for object fields appearing in data constructors

The generated lenses are compatible with the `purescript-profunctor-lenses` library.

## Example

Input file:

```purescript
module Data.Tree where

type Foo = {foo :: Int}

data Tree a
  = Leaf
  | Branch { l :: Tree a, value :: a, r :: Tree a }
```

Output file:

```purescript
module Data.Tree.Lenses where

import Prelude (Unit, unit, const)
import Data.Lens (Lens, PrismP, lens, prism)
import Data.Either (Either(..))
import Data.Tree


foo :: forall a b r. Lens { "foo" :: a | r } { "foo" :: b | r } a b
foo = lens _."foo" (_ { "foo" = _ })

l :: forall a b r. Lens { "l" :: a | r } { "l" :: b | r } a b
l = lens _."l" (_ { "l" = _ })

value :: forall a b r. Lens { "value" :: a | r } { "value" :: b | r } a b
value = lens _."value" (_ { "value" = _ })

r :: forall a b r. Lens { "r" :: a | r } { "r" :: b | r } a b
r = lens _."r" (_ { "r" = _ })

_Leaf :: forall a. PrismP (Tree a) Unit
_Leaf = prism (const Leaf) unwrap
  where
  unwrap Leaf = Right unit
  unwrap y = Left y

_Branch :: forall a. PrismP (Tree a) { r :: Tree a
                                     , value :: a
                                     , l :: Tree a
                                     }
_Branch = prism Branch unwrap
  where
  unwrap (Branch x) = Right x
  unwrap y = Left y
```

These optics can now be composed in the usual ways:

```purescript
leftRight :: forall a. PrismP (Tree a) (Tree a)
leftRight = r <<< _Branch <<< l <<< _Branch
```
