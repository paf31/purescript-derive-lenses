module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Except (runExcept)
import Data.Array (catMaybes, concat, concatMap, groupBy, head, null)
import Data.Either (Either(..))
import Data.Foldable (foldMap, traverse_)
import Data.Foreign (F, ForeignError(..), fail, readArray, readString, renderForeignError)
import Data.Foreign.Index (index)
import Data.Foreign.JSON (parseJSON)
import Data.Foreign.Keys (keys)
import Data.Function (on)
import Data.Maybe (Maybe(..), fromMaybe')
import Data.NonEmpty (NonEmpty(..))
import Data.String (joinWith)
import Data.Traversable (for, traverse)
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.FS.Sync (readTextFile, writeTextFile)
import Node.Process (PROCESS, exit)
import Node.Yargs.Applicative (runY, yarg)
import Node.Yargs.Setup (example)

type DataConstructor =
  { typeName :: String
  , ctorName :: String
  , argNames :: Array String
  }

codegen :: String -> String -> Array DataConstructor -> String
codegen chosenModuleName importedModuleName ctors =
    joinWith "\n" $
      [ "module " <> chosenModuleName <> " where"
      , ""
      , "import Prelude as Prelude"
      , "import Data.Lens as Lens"
      , "import Data.Either as Either"
      , ""
      , "import " <> importedModuleName
      , ""
      ] <> concatMap opticFor typeCtors
  where
    typeCtors = groupBy (eq `on` _.typeName) ctors

    opticFor :: NonEmpty Array DataConstructor -> Array String
    opticFor ctors'@(NonEmpty { typeName } rest) = header <> foldMap opticFor' ctors' where
      header = [ "-- " <> typeName
               , ""
               ]

      opticFor' { ctorName, argNames: [_] }
        | null rest =
            [ "_" <> ctorName <> " = Lens.iso unwrap " <> ctorName <> " where"
            , "  unwrap (" <> ctorName <> " x) = x"
            , ""
            ]
        | otherwise =
            [ "_" <> ctorName <> " = Lens.prism " <> ctorName <> " unwrap where"
            , "  unwrap (" <> ctorName <> " x) = Either.Right x"
            , "  unwrap y = Either.Left y"
            , ""
            ]
      opticFor' { ctorName, argNames: [] }
        | null rest =
            [ "_" <> ctorName <> " = Lens.iso unwrap (Prelude.const " <> ctorName <> ") where"
            , "  unwrap " <> ctorName <> " = Prelude.unit"
            , ""
            ]
        | otherwise =
            [ "_" <> ctorName <> " = Lens.prism (Prelude.const " <> ctorName <> ") unwrap where"
            , "  unwrap " <> ctorName <> " = Either.Right Prelude.unit"
            , "  unwrap y = Either.Left y"
            , ""
            ]
      opticFor' _ = []

process :: Maybe String -> String -> F String
process outputModuleName input = do
  json <- parseJSON input
  moduleNames <- keys json
  case moduleNames of
    [moduleName] -> do
      _module <- index json moduleName
      decls <- readArray =<< index _module "decls"
      let chosenModuleName = fromMaybe' (\_ -> moduleName <> ".Lenses") outputModuleName
      codegen chosenModuleName moduleName <<< concat <$> for decls \decl -> do
        names <- keys decl
        catMaybes <$> for names \name -> do
          ctor <- readArray =<< index decl name
          case ctor of
            [tag, _1, _2, _3] -> do
              tag' <- readString tag
              case tag' of
                "Constructor" -> do
                  typeName <- readString _1
                  ctorName <- readString _2
                  argNames <- traverse readString =<< readArray _3
                  pure (Just { typeName, ctorName, argNames })
                _ -> pure Nothing
            _ -> pure Nothing
    _ -> fail (JSONError "Expected a single module")

app :: forall eff
     . String
    -> String
    -> Maybe String
    -> Eff ( fs :: FS
           , exception :: EXCEPTION
           , process :: PROCESS
           , console :: CONSOLE
           | eff
           ) Unit
app inputFile outputFile moduleName = do
  input <- readTextFile UTF8 inputFile
  case runExcept (process moduleName input) of
    Left errs -> do
      traverse_ (log <<< renderForeignError) errs
      exit 1
    Right output -> writeTextFile UTF8 outputFile output

main :: Eff ( exception :: EXCEPTION
            , console :: CONSOLE
            , process :: PROCESS
            , fs :: FS
            ) Unit
main = do
  let setup = example "derive-lenses -i corefn.json -o Module.purs"
                      "Create Module.purs from corefn.json"
           <> example "derive-lenses -i corefn.json -o Module.purs -m Custom.Module.Name"
                      "Specify a custom module name"

  runY setup $
    app <$> yarg "i"
                 ["input"]
                 (Just "Input corefn.json file")
                 (Right "Input file is required")
                 true
        <*> yarg "o"
                 ["output"]
                 (Just "Output PureScript module")
                 (Right "Output file is required")
                 true
        <*> (head <$> yarg "m"
                      ["module-name"]
                      (Just "Generated module name")
                      (Left [])
                      true)
