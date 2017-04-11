# purescript-derive-lenses

A little utility to derive isos and prisms for data types in PureScript

## Usage

First, build your project with `--dump-corefn`. Then, provide a PureScript
`corefn.json` file with the `-i` argument and an output file with the `-o` argument.

Command:

```bash
# Compile source files with --dump-corefn
purs compile --dump-corefn test-files/Data.Tree.purs

# Build the output module
pulp run -- -i output/Data.Tree/corefn.json -o test-files/Output.purs
```

Available options:

```
-i, --input        Input corefn.json file   [string] [required]
-o, --output       Output PureScript module [string] [required]
-m, --module-name  Generated module name    [string]
```

Using options:

```bash
pulp run -- -i output/Data.Tree/corefn.json -m My.Module -o test-files/My.Module.purs
```

## How it Works

This tool generates `Prism`s/`Iso`s for data constructors which have zero or one argument.

The generated optics are compatible with the `purescript-profunctor-lenses` library.

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

-- Tree

_Leaf = Lens.prism (Prelude.const Leaf) unwrap where
  unwrap Leaf = Either.Right Prelude.unit
  unwrap y = Either.Left y

_Branch = Lens.prism Branch unwrap where
  unwrap (Branch x) = Either.Right x
  unwrap y = Either.Left y
```

These optics can now be composed in the usual ways:

```purescript
leftRight :: forall a. Prism' (Tree a) (Tree a)
leftRight =
  prop (SProxy :: SProxy "r")
  <<< _Branch
  <<< prop (SProxy :: SProxy "l")
  <<< _Branch
```
