# purescript-derive-lenses

A little utility to derive lenses and prisms for data types in PureScript

## Usage

Provide a PureScript module on standard input, and a new module will be returned on standard output

Command:

```
purescript-derive-lenses < ./test-files/Data.Tree.purs > ./test-files/Lenses.purs
```

Available options:
```
--moduleName                  "Name of the output module"
--moduleImports               "Additional imports for the output module separated by | delimiter"
--moduleImportsDelimiter      "Delimiter of additional imports for the output module"
```

Using options:
```
purescript-derive-lenses < ./test-files/Data.Tree.purs --moduleName My.Module --moduleImports "import A|import B" > ./test-files/Lenses.purs
```


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

import Prelude as Prelude
import Data.Lens as Lens
import Data.Either as Either
import Data.Tree
import Data.Bar (Bar)


foo :: forall a b r. Lens.Lens { "foo" :: a | r } { "foo" :: b | r } a b
foo = Lens.lens _."foo" (_ { "foo" = _ })

l :: forall a b r. Lens.Lens { "l" :: a | r } { "l" :: b | r } a b
l = Lens.lens _."l" (_ { "l" = _ })

value :: forall a b r. Lens.Lens { "value" :: a | r } { "value" :: b | r } a b
value = Lens.lens _."value" (_ { "value" = _ })

r :: forall a b r. Lens.Lens { "r" :: a | r } { "r" :: b | r } a b
r = Lens.lens _."r" (_ { "r" = _ })

_Leaf :: forall a. Lens.Prism' (Tree a) Prelude.Unit
_Leaf = Lens.prism (Prelude.const Leaf) unwrap
  where
    unwrap Leaf = Either.Right Prelude.unit
    unwrap y = Either.Left y

_Branch :: forall a.
             Lens.Prism' (Tree a)
               { "l" :: Tree a
               , "value" :: a
               , "r" :: Tree a
               }
_Branch = Lens.prism Branch unwrap
  where
    unwrap (Branch x) = Either.Right x
    unwrap y = Either.Left y
```

These optics can now be composed in the usual ways:

```purescript
leftRight :: forall a. Prism' (Tree a) (Tree a)
leftRight = r <<< _Branch <<< l <<< _Branch
```
