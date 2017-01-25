{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import           Data.Maybe (fromMaybe, mapMaybe)
import           Data.Text (pack, unpack)
import qualified Language.PureScript as P
import           Language.PureScript.Label (runLabel)
import           Language.PureScript.PSString (decodeString)
import           Options.Generic (Generic, ParseRecord, getRecord)
import           Text.PrettyPrint.Boxes ((<>))
import qualified Text.PrettyPrint.Boxes as Box

-- | Options for the command line interface
data Options = Options
  { moduleName :: Maybe String
  -- ^ the name to use for the output module
  } deriving (Generic, Show)

instance ParseRecord Options

-- | An optic to be rendered during code generation.
data Optic
  = DataMemberLens P.Text
  -- ^ a lens for a record accessor
  | DataConstructorPrism (P.ProperName 'P.ConstructorName) P.Type (Maybe P.Type)
  -- ^ a prism for a data constructor (constructor name, outer type, inner type)
  | DataConstructorIso (P.ProperName 'P.ConstructorName) P.Type (Maybe P.Type)
  -- ^ an iso for a newtype (or its data equivalent)
  deriving Show

-- | Turn a module into a list of optics for codegen.
process :: P.Module -> (P.ModuleName, [Optic])
process (P.Module _ _ mn ds _) =
    (mn, concatMap processDecl ds)
  where
    typeAppToLenses (P.TypeApp rec r)
      | P.tyRecord == rec = DataMemberLens <$> mapMaybe (decodeString . runLabel . fst) (fst (P.rowToList r))
    typeAppToLenses _ = []

    processDecl :: P.Declaration -> [Optic]
    processDecl (P.TypeSynonymDeclaration _ _ ta) = typeAppToLenses ta
    processDecl (P.DataDeclaration _ nm args dctors) =
        concatMap member dctors ++ isoOrPrism dctors
      where
        member (_, [ta]) = typeAppToLenses ta
        member _ = []

        isoOrPrism [(dctor, [inner])] = [DataConstructorIso dctor outer (Just inner)]
        isoOrPrism [(dctor, [])] = [DataConstructorIso dctor outer Nothing]
        isoOrPrism _ = mapMaybe ctor dctors

        ctor (dctor, [inner]) = Just (DataConstructorPrism dctor outer (Just inner))
        ctor (dctor, []) = Just (DataConstructorPrism dctor outer Nothing)
        ctor _ = Nothing

        outer :: P.Type
        outer = foldl P.TypeApp
                      (P.TypeConstructor (P.Qualified Nothing nm))
                      (map (P.TypeVar . fst) args)
    processDecl (P.PositionedDeclaration _ _ d) = processDecl d
    processDecl _ = []

-- | Generate PureScript code for the output module.
codeGen :: String -> P.ModuleName -> [Optic] -> Box.Box
codeGen thisModule sourceModule = Box.vsep 1 Box.left . (preamble :) . map renderOptic
  where
    preamble :: Box.Box
    preamble = Box.text . unlines $
      [ "module " ++ thisModule ++ " where"
      , ""
      , "import Prelude as Prelude"
      , "import Data.Lens as Lens"
      , "import Data.Either as Either"
      , "import " ++ (unpack . P.runModuleName) sourceModule
      ]

    renderOptic :: Optic -> Box.Box
    renderOptic (DataMemberLens prop) = Box.vcat Box.left
        [ Box.text (unpack prop ++ " :: forall a b r. Lens.Lens " ++ recTy "a" ++ " " ++ recTy "b" ++ " a b")
        , Box.text (unpack prop ++ " = Lens.lens _." ++ show prop ++ " (_ { " ++ show prop ++ " = _ })")
        ]
      where
        recTy :: String -> String
        recTy ty = "{ " ++ show prop ++ " :: " ++ ty ++ " | r }"
    renderOptic (DataConstructorPrism dctor outer (Just inner)) = Box.vcat Box.left
        [ Box.text (opticName dctor ++ " :: ") <> P.typeAsBox (P.quantify (prismTy inner outer))
        , Box.text (opticName dctor ++ " = Lens.prism " ++ properName dctor ++ " unwrap")
        , Box.text   "  where"
        , Box.text $ "    unwrap (" ++ properName dctor ++ " x) = Either.Right x"
        , Box.text   "    unwrap y = Either.Left y"
        ]
    renderOptic (DataConstructorPrism dctor outer Nothing) = Box.vcat Box.left
        [ Box.text (opticName dctor ++ " :: ") <> P.typeAsBox (P.quantify (prismTy unit outer))
        , Box.text (opticName dctor ++ " = Lens.prism (Prelude.const " ++ properName dctor ++ ") unwrap")
        , Box.text   "  where"
        , Box.text $ "    unwrap " ++ properName dctor ++ " = Either.Right Prelude.unit"
        , Box.text   "    unwrap y = Either.Left y"
        ]
    renderOptic (DataConstructorIso dctor outer (Just inner)) = Box.vcat Box.left
        [ Box.text (opticName dctor ++ " :: ") <> P.typeAsBox (P.quantify (isoTy inner outer))
        , Box.text (opticName dctor ++ " = Lens.iso unwrap " ++ properName dctor)
        , Box.text   "  where"
        , Box.text $ "    unwrap (" ++ properName dctor ++ " x) = x"
        ]
    renderOptic (DataConstructorIso dctor outer Nothing) = Box.vcat Box.left
        [ Box.text (opticName dctor ++ " :: ") <> P.typeAsBox (P.quantify (isoTy unit outer))
        , Box.text (opticName dctor ++ " = Lens.iso unwrap (Prelude.const " ++ properName dctor ++ ")")
        , Box.text   "  where"
        , Box.text $ "    unwrap " ++ properName dctor ++ " = Prelude.unit"
        ]

    unit :: P.Type
    unit = P.TypeConstructor (P.Qualified Nothing (P.ProperName "Prelude.Unit"))

    properName :: P.ProperName 'P.ConstructorName -> String
    properName = unpack . P.runProperName

    opticName :: P.ProperName 'P.ConstructorName -> String
    opticName dctor = '_' : properName dctor

    prismTy :: P.Type -> P.Type -> P.Type
    prismTy = opticTy "Lens.Prism'"

    isoTy :: P.Type -> P.Type -> P.Type
    isoTy = opticTy "Lens.Iso'"

    opticTy :: P.Text -> P.Type -> P.Type -> P.Type
    opticTy name inner outer = _Type `P.TypeApp` outer `P.TypeApp` inner
      where
        _Type :: P.Type
        _Type = P.TypeConstructor (P.Qualified Nothing (P.ProperName name))

-- | Transforms a source module (as text) into a module full of lenses (as text).
app :: Options -> String -> String
app Options{..} input =
    case P.parseModuleFromFile id ("<input>", pack input) of
      Left errs -> show errs
      Right (_, m) -> (P.renderBox . uncurry codeGen' . process) m
  where
    codeGen' sourceModule = codeGen (fromMaybe ((unpack . P.runModuleName) sourceModule ++ ".Lenses") moduleName) sourceModule

-- | 'main' is a wrapper for the 'app' function
main :: IO ()
main = do
  opts <- getRecord "purescript-derive-lenses"
  interact (app opts)
