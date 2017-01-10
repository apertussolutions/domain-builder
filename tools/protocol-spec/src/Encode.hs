{-# LANGUAGE QuasiQuotes, PackageImports #-}
module Encode where

import CTypes
import AST
import Common
import Text.PrettyPrint

import Data.Hash
import qualified Data.Map as Map

import Language.C.Quote.GCC
import qualified "language-c-quote" Language.C.Syntax as S

{-
The state used by the encoder functions:

heap_end:  points just past the end of the target buffer.
nextObj:   points to the place in the target where we should place the next obj
cur:       points to the object in the target heap that is being encoded.
src:       point to the object in the source heap, that we are encoding.

The algorithm works as follows:

1. Copy an object from the source to the target heap
      (from `src` to `nextObj`)
2. Set `cur` to point to the new object in the target heap (i.e., `nextObj`)
3. Update `nextObj` to point after the new object.
4. Process subcomponents of `cur` that need encoding (i.e., pointers).
-}


helper :: Helper
helper = Helper
  { helperDef  = helperFun
  , helperDecl = \d ->
      [cedecl|static int $id:(helperName d)(void*,void*,void**); |]
  , helperOther = [encodeString]
  }


helperName :: Ident -> String
helperName i = "__encode_" ++ identString i


helperFun :: Env -> Decl -> Maybe S.Definition
helperFun env decl =
  onlyIf (encodeContent env (Just name) (dType decl)) $ \subCheck ->
    Just $
    [cedecl|
    static
    int $id:(helperName name) (void *heap_end, void *cur, void **nextObjAddr) {
      void *nextObj = *nextObjAddr;

      $stms:subCheck
      *nextObjAddr = nextObj;
      return 0;
    } |]
  where
  name = dName decl


encodeFun :: Env -> Codec -> (Doc, Doc)
encodeFun env c =
  ( text $ showP $
      [cedecl| int $id:funName ($ty:tyC *, void *, typename size_t); |]
  , text $ showP mainFun
  )
  where
  mainFun =
    [cedecl|

      int $id:funName ($ty:tyC *src, void *nextObj, typename size_t size) {

        void *heap_end = nextObj + size;
        void *cur;

        if (size < 8) return -9;
        * (typename uint64_t*) nextObj = $ullint:(asWord64 h);
        nextObj += 8;
        cur = nextObj;
        $stm:(newObject ty)
        $stms:(encodeContent env Nothing ty)
        return (size - (heap_end - nextObj));
      }
    |]

  tyC               = cType ty
  funName           = identString (encodeName c)
  ty                = Named (codecType c)
  Just (_,Just h,_) = Map.lookup (codecType c) (envDecls env)



encodeString :: S.Definition
encodeString =
  [cedecl|
    static inline
    int __copy_string(char *src, void** nextObjAddr, void* heap_end)
      { void *nextObj = *nextObjAddr;

        for (; *src != '\0' && nextObj < heap_end; ++src, ++nextObj)
          *(char*)nextObj = *src;

        if (nextObj < heap_end) {
          *(char*)nextObj = '\0';
          ++nextObj;
        }

        if (nextObj > heap_end) return -1;
        *nextObjAddr = nextObj;
        return 0;
      }
  |]


newObject :: Type -> S.Stm
newObject ty =
  case ty of

    String -> [cstm|
      { int err = __copy_string(src, &nextObj, heap_end);
        if (err != 0) return err;
      } |]


    Array t sz -> [cstm|
      { typename size_t size = $(arrSize sz) * sizeof($ty:(cTypeSize t));
        if (size > heap_end - nextObj) return -2;
        memcpy(nextObj,src,size);
        nextObj += size;
      } |]


    _ -> [cstm|
      { typename size_t size = sizeof($ty:(cTypeSize ty));
        if (size > heap_end - nextObj) return -2;
        memcpy(nextObj,src,size);
        nextObj += size;
      } |]


checkStructField :: Env -> Field -> [S.BlockItem]
checkStructField ds f
  -- nothing to check for numeric types
  | isIndexType ds (fType f) =
        [ S.BlockDecl [cdecl| typename size_t $id:n = obj->$id:fld; |] ]
  | otherwise = onlyIf (encodeContent ds Nothing (fType f)) $ \subCheck ->
                    map S.BlockStm $ [cstm| cur = &obj->$id:fld; |] : subCheck
  where
  n     = sizeAttrName (fName f)
  fld   = identString (fName f)

checkUnionField :: Env -> String -> Field -> S.Stm
checkUnionField ds tag f =
  [cstm| case $id:(enumLabel tag f): { $stms:sub break; } |]
  where
  sub = onlyIf (encodeContent ds Nothing (fType f)) $ \subCheck ->
            [cstm| cur = &obj->u; |] : subCheck

encodeContent :: Env -> Maybe Ident -> Type -> [S.Stm]
encodeContent env mbName ty =
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

    {- WARNING:  Here we ASSUME that the input is not NULL,
    which is dual to the behavior of the decoder, where we GUARANTEE
    that the value is not NULL.  Note that this means that if the
    encoder is applied to malformed input (e.g., a NULL value that's
    supposed to point to something), the encoder will segfault.
    This is not unique to this case:  other pointers that point to
    bad memory will also lead to a segfault. -}
    Reference t ->
      [ [cstm| { $ty:myT src                 = * (void**) cur;
                 * (typename uintptr_t*) cur = -1; // holds no info.
                 cur                         = nextObj;
                 $stm:(newObject t)
                 $stms:(encodeContent env Nothing t)
               }
        |]
      ]

    Pointer t ->
      [ [cstm| if (* (void**) cur != NULL) {
                  $ty:myT src                 = * (void**) cur;
                  * (typename uintptr_t*) cur = -1;
                  cur                         = nextObj;
                  $stm:(newObject t)
                  $stms:(encodeContent env Nothing t)
               }
        |]
      ]

    Array t s -> onlyIf (encodeContent env Nothing t) $ \subCheck ->
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


  where
  myT = case mbName of
          Just t  -> [cty| typename $id:(identString t) |]
          Nothing -> cType ty









