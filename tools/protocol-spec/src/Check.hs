{-# LANGUAGE DeriveFunctor #-}
module Check where

import AST

check :: [Ident] -> Type -> [String]
check locals t
  = case t of
      PrimType _   -> []
      Pointer ty   -> check locals ty
      Reference ty -> check locals ty
      Named _      -> []
      String       -> []
      Struct fs    -> checkFields [] fs
      Union _ fs   -> checkFields [] fs
      Array ty sz  -> checkArray locals ty sz

checkFields :: [Ident] -> [Field] -> [String]
checkFields _      [] = []
checkFields locals (f:fs)
  | fName f `elem` locals = ["duplicate field " ++ identName (fName f)]
                               ++ check locals (fType f) ++ checkFields locals fs
  | otherwise             =       check locals (fType f) ++ checkFields (fName f : locals) fs

checkArray :: [Ident] -> Type -> Size -> [String]
checkArray locals ty (FixedSize _) = check locals ty
checkArray locals ty (VarSize var)
  | var `notElem` locals = ["bad array length " ++ identName var] ++ check locals ty
  | otherwise            =                         check locals ty

identName :: Ident -> String
identName (I n) = n
