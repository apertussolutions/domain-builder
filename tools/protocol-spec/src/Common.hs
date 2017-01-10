{-# LANGUAGE QuasiQuotes, PackageImports #-}
module Common where

import AST

import Data.Hash
import Control.Monad(MonadPlus,mzero)
import Language.C.Quote.GCC
import qualified "language-c-quote" Language.C.Syntax as S
import qualified Data.Map as Map
import Text.PrettyPrint

import qualified Text.PrettyPrint.Mainland as MP

{- The environment map associates type names with:
   1. Their defintion
   2. A hash, which should be defined for types with top-level encoder/decoders
   3. A function that will encode/decode this type, once allocated.
      Some named types don't need any special work to be done, which is
      why this field is optional.
-}

newtype Env = Env { envDecls :: Map.Map Ident
                                  (Decl, Maybe Hash, Maybe S.Definition) }

sizeAttrName :: Ident -> String
sizeAttrName i = "__" ++ identString i

arrSize :: Size -> S.Exp
arrSize (FixedSize n) = [cexp| $n |]
arrSize (VarSize f)   = [cexp| $id:(sizeAttrName f) |]

onlyIf :: MonadPlus m => [a] -> ([a] -> m b) -> m b
onlyIf [] _ = mzero
onlyIf ss f = f ss

isIndexType :: Env -> Type -> Bool
isIndexType env ty =
  case ty of
    PrimType s  -> s `elem` [ "unsigned int" ]
    Named n     -> isIndexType env $ dType $ getDecl env n
    _           -> False

hasHelper :: Env -> Ident -> Bool
hasHelper env x = case Map.lookup x (envDecls env) of
                    Just (_,_,Just _) -> True
                    _                 -> False

getDecl :: Env -> Ident -> Decl
getDecl env name = case Map.lookup name (envDecls env) of
                     Just (d,_,_)  -> d
                     Nothing -> error ("Undefined type: " ++ show name)

simpleEnv :: [Decl] -> Env
simpleEnv ds = Env { envDecls = Map.fromList (map entry ds) }
  where entry d = (dName d, (d, Nothing, Nothing))

--------------------------------------------------------------------------------
-- Infrastructure for helper functions, which is used by both
-- encoders and decoders.


data Helper = Helper
  { helperDef   :: Env -> Decl -> Maybe S.Definition
  , helperDecl  :: Ident -> S.Definition
  , helperOther :: [S.Definition]   -- These go first
  }

{- NOTE: Here we use "the lazyness trick" where `env` is used and defined
simulataneously.  This works because the declarations cannot be recursive,
so everything will be defined before it is used. -}
helperFuns :: Helper -> Spec -> (Doc, Env)
helperFuns p spec = ( vcat $ map (text . showP) (helperOther p ++ decls ++ defs)
                    , env)
  where
  env           = Env (Map.fromList (map mapEntry (typeDecls spec)))

  mapEntry d    = let name = dName d
                  in (name, (d, lookup name (hashes spec), helperDef p env d))

  (decls,defs)  = unzip $ do (d,(_,_,Just def)) <- Map.toList (envDecls env)
                             return (helperDecl p d,def)


enumLabel :: String -> Field -> String
enumLabel tag f = tag ++ identString (fName f)

showP :: MP.Pretty a => a -> String
showP x = MP.pretty 80 (MP.ppr x)
