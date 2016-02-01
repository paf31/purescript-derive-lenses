{-# LANGUAGE PatternGuards #-}

module Main where

import Data.Maybe (mapMaybe)

import Control.Applicative

import qualified Language.PureScript as P

import Text.PrettyPrint.Boxes ((<>))
import qualified Text.PrettyPrint.Boxes as Box

data Optic
  = DataMemberLens String
  | DataConstructorPrism P.ProperName P.Type (Maybe P.Type)
  deriving Show

parse :: String -> Either P.MultipleErrors [P.Module]
parse s = map snd <$> P.parseModulesFromFiles id [("input", s)]

process :: P.Module -> (P.ModuleName, [Optic])
process (P.Module _ _ mn ds _) = (mn, concatMap processDecl ds)
  where
  typeAppToLenses (P.TypeApp obj r)
      | P.tyObject == obj = map (DataMemberLens . fst) (fst (P.rowToList r))
  typeAppToLenses _ = []
  processDecl :: P.Declaration -> [Optic]
  processDecl (P.TypeSynonymDeclaration _ _ ta) = typeAppToLenses ta
  processDecl (P.DataDeclaration _ nm args dctors) =
    concatMap member dctors ++ mapMaybe ctor dctors
    where
    member (_, [ta]) = typeAppToLenses ta
    member _ = []

    ctor (dctor, [inner]) = Just (DataConstructorPrism dctor outer (Just inner))
    ctor (dctor, []) = Just (DataConstructorPrism dctor outer Nothing)
    ctor _ = Nothing

    outer :: P.Type
    outer = foldl P.TypeApp (P.TypeConstructor (P.Qualified Nothing nm)) (map (P.TypeVar . fst) args)
  processDecl (P.PositionedDeclaration _ _ d) = processDecl d
  processDecl _ = []

printModule :: P.ModuleName -> [Optic] -> Box.Box
printModule mn = Box.vsep 1 Box.left . (preamble :) . map renderOptic
  where
  preamble :: Box.Box
  preamble = Box.text . unlines $
    [ "module " ++ P.runModuleName mn ++ ".Lenses where"
    , ""
    , "import Prelude (Unit, unit, const)"
    , "import Data.Lens (Lens, PrismP, lens, prism)"
    , "import Data.Either (Either(..))"
    , "import " ++ P.runModuleName mn
    ]

  renderOptic :: Optic -> Box.Box
  renderOptic (DataMemberLens prop) = Box.vcat Box.left
    [ Box.text (prop ++ " :: forall a b r. Lens " ++ recTy "a" ++ " " ++ recTy "b" ++ " a b")
    , Box.text (prop ++ " = lens _." ++ show prop ++ " (_ { " ++ show prop ++ " = _ })")
    ]
    where
    recTy :: String -> String
    recTy ty = "{ " ++ show prop ++ " :: " ++ ty ++ " | r }"
  renderOptic (DataConstructorPrism dctor outer (Just inner)) = Box.vcat Box.left
    [ Box.text (name ++ " :: ") <> P.typeAsBox (P.quantify (prismTy inner outer))
    , Box.text (name ++ " = prism " ++ P.runProperName dctor ++ " unwrap")
    , Box.text   "  where"
    , Box.text $ "  unwrap (" ++ P.runProperName dctor ++ " x) = Right x"
    , Box.text   "  unwrap y = Left y"
    ]
    where
    name :: String
    name = '_' : P.runProperName dctor
  renderOptic (DataConstructorPrism dctor outer Nothing) = Box.vcat Box.left
    [ Box.text (name ++ " :: ") <> P.typeAsBox (P.quantify (prismTy unit outer))
    , Box.text (name ++ " = prism (const " ++ P.runProperName dctor ++ ") unwrap")
    , Box.text   "  where"
    , Box.text $ "  unwrap " ++ P.runProperName dctor ++ " = Right unit"
    , Box.text   "  unwrap y = Left y"
    ]
    where
    unit :: P.Type
    unit = P.TypeConstructor (P.Qualified Nothing (P.ProperName "Unit"))

    name :: String
    name = '_' : P.runProperName dctor

  prismTy :: P.Type -> P.Type -> P.Type
  prismTy inner outer = _Prism `P.TypeApp` outer `P.TypeApp` inner
    where
    _Prism :: P.Type
    _Prism = P.TypeConstructor (P.Qualified Nothing (P.ProperName "PrismP"))

app :: String -> String
app input =
  case parse input of
    Left errs -> show errs
    Right ms -> P.renderBox $ Box.vsep 1 Box.left (map (uncurry printModule . process) ms)

main = interact app
