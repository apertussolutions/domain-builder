{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PackageImports #-}
module CTypes where

import AST
import Common

import qualified "language-c-quote" Language.C.Syntax as C
import Language.C.Quote.GCC

cType :: Type -> C.Type
cType ty =
  case ty of
    PrimType t  -> [cty| typename $id:t |]
    Pointer t   -> [cty| $ty:(cPtrType t) * |]
    Reference t -> [cty| $ty:(cPtrType t) * |]
    Named x     -> [cty| typename $id:(identString x) |]

    Struct fs  ->
      [cty| struct __attribute__((packed)) { $sdecls:(map cField fs) } |]

    Union tag fs ->
      [cty| struct __attribute__((packed)) {
              $ty:enum tag;
              union __attribute__((packed)) { $sdecls:(map cField fs) } u;
            }
      |]
      where
      enum    = [cty| enum { $enums:(map enumL fs) } |]
      enumL x = [cenum| $id:(enumLabel tag x)  |]

    Array t (FixedSize n) -> [cty| $ty:(cType t) [$n] |]

    -- These should always appear under a pointer.
    String                -> error "Unexpected naked string."
    Array _ (VarSize   _) -> error "Unexpected naked var. sized array."

cPtrType :: Type -> C.Type
cPtrType ty =
  case ty of
    String                -> [cty| char |]
    Array t (VarSize   _) -> cType t
    _                     -> cType ty

-- Get a type that has the same size (but is possibly shorter) as the original
cTypeSize :: Type -> C.Type
cTypeSize ty =
  case ty of
    PrimType t  -> [cty| typename $id:t |]
    Pointer _   -> [cty| void* |]
    Reference _ -> [cty| void* |]
    Named x     -> [cty| typename $id:(identString x) |]

    Struct fs  ->
      [cty| struct __attribute__((packed)) { $sdecls:(map szField fs) } |]

    Union _ fs ->
      [cty| struct __attribute__((packed)) {
              enum { TAG } tag;
              union __attribute__((packed))__ { $sdecls:(map szField fs) } u;
            }
      |]

    Array t (FixedSize n) -> [cty| $ty:(cTypeSize t) [$n] |]

    -- These should always appear under a pointer.
    String                -> error "Unexpected naked string."
    Array _ (VarSize   _) -> error "Unexpected naked var. sized array."

  where
  szField f =
      [csdecl| $ty:(cTypeSize (fType f)) $id:(identString (fName f)); |]



cField :: Field -> C.FieldGroup
cField f = [csdecl| $ty:(cType (fType f)) $id:(identString (fName f)); |]

cDecl :: Decl -> C.Definition
cDecl d =
  [cedecl| typedef $ty:(cType (dType d)) $id:(identString (dName d));|]



