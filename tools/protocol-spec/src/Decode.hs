{-# LANGUAGE QuasiQuotes, PackageImports #-}
module Decode(decodeFun,helper) where


import AST
import CTypes
import Common

import qualified Data.Map as Map
import Data.Hash
import Text.PrettyPrint

import Language.C.Quote.GCC
import qualified "language-c-quote" Language.C.Syntax as S


helper :: Helper
helper = Helper
  { helperDef  = helperFun
  , helperDecl = \d ->
      [cedecl| static int $id:(helperName d)(void*,void*,void**); |]
  , helperOther = []
  }

helperName :: Ident -> String
helperName i = "__decode_" ++ identString i

helperFun :: Env -> Decl -> Maybe S.Definition
helperFun env decl =
  onlyIf (checkContent env (Just name) (dType decl)) $ \subCheck ->
  Just $
  [cedecl|
  static
  int $id:(helperName name) (void *heap_end, void *cur, void **nextObjAddr) {
    void *nextObj = *nextObjAddr;
    typename size_t size;

    $stms:subCheck
    *nextObjAddr = nextObj;
    return 0;
  } |]
  where name = dName decl

decodeFun :: Env -> Codec -> (Doc, Doc)
decodeFun env c =
  ( text $ showP $
      [cedecl| int $id:funName (void **buf, typename size_t buf_size); |]
  , text $ showP mainFun
  )
  where
  mainFun =
    [cedecl|

      int $id:funName (void **buf, typename size_t buf_size) {

        typename size_t size;
        void *heap_end = *buf + buf_size;
        void *cur = *buf;
        void *nextObj = cur + 8;

        if (buf_size < 8) return -9;
        if (* (typename uint64_t*) cur != $ullint:(asWord64 h)) return -10;


        *buf = nextObj;
        cur  = nextObj;
        $stms:(newObject ty)
        $stms:(checkContent env Nothing ty)
        return 0;
      }
    |]

  funName           = identString (decodeName c)
  ty                = Named (codecType c)
  Just (_,Just h,_) = Map.lookup (codecType c) (envDecls env)

--------------------------------------------------------------------------------

newObject :: Type -> [S.Stm]
newObject ty =
  case ty of
    String ->
      [ [cstm| size = strnlen(nextObj, heap_end - nextObj); |]
      , [cstm| if (size == heap_end - nextObj) return -4;   |]
      , [cstm| nextObj += (size + 1);                       |]
      ]

    Array t sz ->
      [ [cstm| size = $(arrSize sz) * sizeof($ty:(cTypeSize t)); |]
      , [cstm| if (size > heap_end - nextObj) return -5;          |]
      , [cstm| nextObj += size;                                   |]
      ]

    _ ->
      [ [cstm| size = sizeof($ty:(cTypeSize ty));        |]
      , [cstm| if (size >  heap_end - nextObj) return -6; |]
      , [cstm| nextObj += size;                           |]
      ]

checkStructField :: Env -> Field -> [S.BlockItem]
checkStructField ds f
  -- nothing to check for numeric types
  | isIndexType ds (fType f) =
        [ S.BlockDecl [cdecl| typename size_t $id:n = obj->$id:fld; |] ]
  | otherwise = onlyIf (checkContent ds Nothing (fType f)) $ \subCheck ->
                    map S.BlockStm $ [cstm| cur = &obj->$id:fld; |] : subCheck
  where
  n     = sizeAttrName (fName f)
  fld   = identString (fName f)

checkUnionField :: Env -> String -> Field -> S.Stm
checkUnionField ds tag f =
  [cstm| case $id:(enumLabel tag f) : { $stms:sub break; } |]
  where
  sub = onlyIf (checkContent ds Nothing (fType f)) $ \subCheck ->
            [cstm| cur = &obj->u; |] : subCheck

-- Assuming that `cur` is a pointer that points to an object that
-- we've alray checked fits in the heap.
checkContent :: Env -> Maybe Ident -> Type -> [S.Stm]
checkContent env mbName ty =
  case ty of

    String     -> []
    PrimType _ -> []

    Named n
      | hasHelper env n ->
        [ [cstm| { int err = $id:(helperName n)(heap_end,cur,&nextObj);
                   if (err != 0) return err;
                 } |]
        ]
      | otherwise -> []


    Array t s -> onlyIf (checkContent env Nothing t) $ \subCheck ->
      [ [cstm| { $ty:(cType t) *obj = cur;
                 int i;
                 for (i = 0; i < $(arrSize s); ++i) {
                    cur = obj + i;
                    $stms:subCheck
                  }
                } |]
      ]

    Struct fs -> onlyIf (concatMap (checkStructField env) fs) $ \checks ->
      [ [cstm| { $items:(me:checks) } |]
      ]
      where
      me = S.BlockDecl [cdecl| $ty:myT *obj = cur; |]

    Union tag fs ->
      [ [cstm| { $ty:myT *obj = cur;
                 switch (obj->tag) {
                   $stms:(map (checkUnionField env tag) fs ++ [deflt])
                 }
               } |]
      ]
      where
      deflt = [cstm| default: return -2; |]

    Reference t ->
      [ [cstm| *((void**)cur) = nextObj; |]
      , [cstm| cur            = nextObj; |]
      ] ++ newObject t
        ++ checkContent env Nothing t

    Pointer t ->
      [ [cstm| if (*(void**) cur != NULL) {
                  *((void**)cur) = nextObj;
                  cur            = nextObj;
                  $stms:(newObject t)
                  $stms:(checkContent env Nothing t)
               } |]
      ]

  where
  myT = case mbName of
          Just t  -> [cty| typename $id:(identString t) |]
          Nothing -> cType ty




