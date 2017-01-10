{-# LANGUAGE PackageImports #-}
module Parse where


import AST
import Check

import Data.Hash
import Data.List
import Data.Maybe
import "language-c" Language.C
import Language.C.Analysis
import MonadLib
import qualified Data.Map as Map

systemVersion :: Int
systemVersion = 2

type M = ReaderT (Map.Map SUERef TagDef) (ExceptionT String Id)

hasher :: [AST.Decl] -> AST.Type -> Hash
hasher decls ty = hash systemVersion `combine` hashType decls ty

raiseAt :: NodeInfo -> String -> M a
raiseAt ni str = raise (show (posOfNode ni) ++ ": " ++ str)

toSpec :: CTranslUnit -> Either String Spec
toSpec u
  = runId . runExceptionT
  $ case runTrav_ (analyseAST u) of
      Left errs     -> raise (show errs)
      Right (x, []) -> runReaderT (gTags x)
        $ do let localTypes = filter notBuiltin (Map.assocs (gTypeDefs x))
             xs <- mapM toDecl localTypes
             let (cs,ds) = unzip xs
             let hs = [(dName d, hasher ds (dType d)) | d <- ds]
             return Spec { codecs    = catMaybes cs
                         , typeDecls = ds
                         , hashes    = hs
                         }
      Right (_, errs) -> raise (show errs)


notBuiltin :: (Language.C.Ident, TypeDef) -> Bool
notBuiltin (ident, _) = not ("__" `isPrefixOf` identToString ident )

toIdent :: Language.C.Ident -> AST.Ident
toIdent x = I (identToString x)

toDecl :: (Language.C.Ident, TypeDef) -> M (Maybe Codec, AST.Decl)
toDecl (ident, TypeDef _ ty attrs ni)
  = do t <- toType ni ty
       case check [] t of
         [] -> return ()
         es -> raiseAt ni ("Bad! " ++ intercalate ", " es)
       mbc <- attrsToCodec ident attrs
       return (mbc, AST.Decl { dName = toIdent ident , dType = t })

attrsToCodec :: Language.C.Ident -> [Attr] -> M (Maybe Codec)
attrsToCodec _ [] = return Nothing
attrsToCodec ident [Attr attr args ni]
  | identToString attr == "codec"
  = case args of
      []                       -> return $ Just Codec { encodeName = I "encode"
                                                      , decodeName = I "decode"
                                                      , codecType = toIdent ident}
      [CVar enc _, CVar dec _] -> return $ Just Codec { encodeName = toIdent enc
                                                      , decodeName = toIdent dec
                                                      , codecType = toIdent ident}
      _                        -> raiseAt ni "Bad arguments to mkcodec"
attrsToCodec _ (Attr _ _ ni:_) = raiseAt ni "Bad attributes"

unionAttrs :: [Attr] -> M String
unionAttrs [] = return ""
unionAttrs [Attr attr args ni]
  | identToString attr == "tagprefix"
  = case args of
      [CVar tag _] -> return (identToString tag)
      _            -> raiseAt ni "Bad arguments to tagprefix"
unionAttrs (Attr _ _ ni:_) = raiseAt ni "Bad attribute"

fromComp :: CompTypeRef -> M AST.Type
fromComp (CompTypeRef ref _ ni)
  = do Just ty <- fmap (Map.lookup ref) ask
       case ty of
         EnumDef _ -> raiseAt ni "Enum not supported"
         CompDef (CompType _ kind members attrs _) ->
           do tag <- unionAttrs attrs
              xs <- mapM toField members
              let con = case kind of
                          StructTag -> Struct
                          UnionTag  -> Union tag
              return (con xs) 

fromVarName :: NodeInfo -> VarName -> M AST.Ident
fromVarName ni n
  = case n of
      NoName -> raiseAt ni "Names required"
      VarName ident asm ->
        do unless (isNothing asm) (raiseAt ni "Assembly names not supported")
           return (toIdent ident)

toField :: MemberDecl -> M Field
toField f
  = case f of
      AnonBitField _ _ _ -> raise "Bit fields not supported"
      MemberDecl (VarDecl name _attrs ty) mbe ni ->
        do unless (isNothing mbe) (raise "Initializers not supported")
           ident <- fromVarName ni name
           t     <- toType ni ty
           return Field { fName = ident, fType = t }

toType :: NodeInfo -> Language.C.Analysis.Type -> M AST.Type
toType ni x@(DirectType b qual [])
  = do unless (qual == noTypeQuals) (raiseAt ni "Bad qualifiers")
       let str = show (pretty x)
       case b of
         TyIntegral _ -> return (PrimType str)
         TyFloating _ -> return (PrimType str)
         TyComp comp  -> fromComp comp
         _            -> raiseAt ni ("Unsupported direct type " ++ str)

toType ni (PtrType (DirectType (TyIntegral TyChar) qual1 []) qual [])
  = do unless (qual  == noTypeQuals) (raiseAt ni "Bad qualifiers")
       unless (qual1 == noTypeQuals) (raiseAt ni "Bad qualifiers")
       return (Pointer String)

toType ni (PtrType ty qual [])
  = do unless (qual == noTypeQuals) (raiseAt ni "Bad qualifiers")
       Pointer `fmap` toType ni ty

toType ni (ArrayType ty sz qual [])
  = do unless (qual == noTypeQuals) (raiseAt ni "Bad qualifiers")
       t <- toType ni ty
       case sz of
         UnknownArraySize _  -> raiseAt ni "Array types must have a size"
         ArraySize True _    -> raiseAt ni "Static array size not supported"
         ArraySize False sze -> case sze of
           CConst (CIntConst i _) -> return (Array t (FixedSize (fromInteger (getCInteger i))))
           CVar ident _           -> return (Reference (Array t (VarSize (toIdent ident))))
           _                      -> raiseAt ni ("Unsupported array size " ++ show sze)

toType ni (TypeDefType (TypeDefRef ident _ _) qual [])
  = do unless (qual == noTypeQuals) (raiseAt ni "Bad qualifiers")
       return (Named (toIdent ident))

toType ni ty = raiseAt ni ("Type not supported: " ++ show (pretty ty))
