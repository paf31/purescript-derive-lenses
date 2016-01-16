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

data Tree a
  = Leaf
  | Branch { l :: Tree a, value :: a, r :: Tree a }
```

Output file:

```purescript
module Data.Tree.Lenses where

l :: forall a b r. Data.Lens.Lens { "l": a } { "l": b } a b
l = Data.Lens.lens _."l" (_ { "l" = _ })

value :: forall a b r. Data.Lens.Lens { "value": a } { "value": b } a b
value = Data.Lens.lens _."value" (_ { "value" = _ })

r :: forall a b r. Data.Lens.Lens { "r": a } { "r": b } a b
r = Data.Lens.lens _."r" (_ { "r" = _ })

_Leaf :: forall a. Data.Lens.PrismP (Data.Tree.Tree a) Unit
_Leaf = Data.Lens.prism (Prelude.const Data.Tree.Leaf) unwrap
  where
  unwrap Data.Tree.Leaf = Data.Either.Right Prelude.unit
  unwrap y = Data.Either.Left y

_Branch :: forall a. Data.Lens.PrismP (Data.Tree.Tree a)
                                      { r :: Data.Tree.Tree a
                                      , value :: a
                                      , l :: Data.Tree.Tree a
                                      }
_Branch = Data.Lens.prism Data.Tree.Branch unwrap
  where
  unwrap (Data.Tree.Branch x) = Data.Either.Right x
  unwrap y = Data.Either.Left y
```

These optics can now be composed in the usual ways:

```purescript
leftRight :: forall a. PrismP (Tree a) (Tree a)
leftRight = r <<< _Branch <<< l <<< _Branch
```
