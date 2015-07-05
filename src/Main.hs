{-# LANGUAGE PatternGuards #-}

module Main where
    
import Data.Maybe (mapMaybe)
    
import Control.Applicative
    
import qualified Language.PureScript as P

data Optic 
  = DataMemberLens String
  | DataConstructorPrism P.ProperName P.Type (Maybe P.Type)
  deriving Show

parse :: String -> Either P.MultipleErrors [P.Module]
parse s = map snd <$> P.parseModulesFromFiles id [("input", s)]

process :: P.Module -> (P.ModuleName, [Optic])
process (P.Module _ mn ds _) = (mn, concatMap processDecl ds)
  where
  processDecl :: P.Declaration -> [Optic]
  processDecl (P.DataDeclaration _ nm args dctors) = 
    concatMap member dctors ++ mapMaybe ctor dctors
    where 
    member (_, [P.TypeApp obj r]) 
      | P.tyObject == obj = map (DataMemberLens . fst) (fst (P.rowToList r))
    member _ = []
    
    ctor (dctor, [inner]) = Just (DataConstructorPrism dctor outer (Just inner))
    ctor (dctor, []) = Just (DataConstructorPrism dctor outer Nothing)
    ctor _ = Nothing
    
    outer :: P.Type
    outer = foldl P.TypeApp (P.TypeConstructor (P.Qualified Nothing nm)) (map (P.TypeVar . fst) args)
  processDecl (P.PositionedDeclaration _ _ d) = processDecl d
  processDecl _ = []
  
printModule :: P.ModuleName -> [Optic] -> String
printModule mn = unlines . (preamble ++) . concatMap printLens
  where
  preamble :: [String]
  preamble = 
    [ "module " ++ show mn ++ ".Lenses where"
    , ""
    , "import Prelude"
    , ""
    , "import Data.Either (Either(..))"
    , "import Data.Profunctor (dimap)"
    , "import Data.Profunctor.Choice (Choice, right)"
    , ""
    , "import " ++ show mn
    , ""
    ]
      
  printLens :: Optic -> [String]
  printLens (DataMemberLens prop) =
    [ prop ++ " :: " ++ P.prettyPrintType lensTy
    , prop ++ " f o = " ++ P.prettyPrintValue lens
    , ""
    ]
    where
    lensTy :: P.Type
    lensTy = P.quantify (P.ConstrainedType [functorF] (coalg a `P.function` coalg rec))
      where
      f :: P.Type
      f = P.TypeVar "f"
      
      r :: P.Type
      r = P.TypeVar "r"
      
      a :: P.Type
      a = P.TypeVar "a"
      
      functorF :: P.Constraint
      functorF = (P.Qualified Nothing (P.ProperName "Functor"), [f])
      
      rec :: P.Type
      rec = P.tyObject `P.TypeApp` P.RCons prop a r
      
      coalg :: P.Type -> P.Type
      coalg ty = ty `P.function` (f `P.TypeApp` ty)
    
    lens :: P.Expr
    lens = P.App (P.App _map updater) (P.App f (P.Accessor prop o))
      where
      f :: P.Expr
      f = P.Var (P.Qualified Nothing (P.Ident "f"))
      
      o :: P.Expr
      o = P.Var (P.Qualified Nothing (P.Ident "o"))
      
      _map :: P.Expr
      _map = P.Var (P.Qualified Nothing (P.Ident "map"))
      
      updater :: P.Expr
      updater = P.ObjectUpdater (Just o) [(prop, Nothing)] 
  printLens (DataConstructorPrism dctor outer (Just inner)) =
    [ name ++ " :: " ++ P.prettyPrintType (prismTy inner outer)
    , name ++ " p = dimap unwrap rewrap (right p)"
    , "  where"
    , "  unwrap (" ++ show dctor ++ " x) = Right x"
    , "  unwrap y = Left y"
    , "  rewrap (Left y) = pure y"
    , "  rewrap (Right x) = map " ++ show dctor ++ " x"
    , ""
    ]
    where
    name :: String
    name = '_' : show dctor 
  printLens (DataConstructorPrism dctor outer Nothing) =
    [ name ++ " :: " ++ P.prettyPrintType (prismTy unit outer)
    , name ++ " p = dimap unwrap (pure <<< rewrap) (right p)"
    , "  where"
    , "  unwrap " ++ show dctor ++ " = Right unit"
    , "  unwrap y = Left y"
    , "  rewrap (Left y) = y"
    , "  rewrap (Right _) = " ++ show dctor
    , ""
    ]
    where
    unit :: P.Type
    unit = P.TypeConstructor (P.Qualified Nothing (P.ProperName "Unit"))   
        
    name :: String
    name = '_' : show dctor    
    
  prismTy :: P.Type -> P.Type -> P.Type
  prismTy inner outer = P.quantify (P.ConstrainedType [applicativeF, choiceP] (coalg inner `P.function` coalg outer))
    where
    f :: P.Type
    f = P.TypeVar "f"
    
    p :: P.Type
    p = P.TypeVar "p"
    
    applicativeF :: P.Constraint
    applicativeF = (P.Qualified Nothing (P.ProperName "Applicative"), [f])
    
    choiceP :: P.Constraint
    choiceP = (P.Qualified Nothing (P.ProperName "Choice"), [p])
    
    coalg :: P.Type -> P.Type
    coalg ty = P.TypeApp (P.TypeApp p ty) (P.TypeApp f ty)

app :: String -> String
app input = 
  case parse input of
    Left errs -> show errs
    Right ms -> unlines (map (uncurry printModule . process) ms)

main = interact app