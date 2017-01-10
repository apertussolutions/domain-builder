{-# LANGUAGE QuasiQuotes, PackageImports #-}
module Pretty where

import CTypes
import AST
import Common
import Language.C.Quote.GCC
import qualified "language-c-quote" Language.C.Syntax as S


prettyFunName :: Ident -> String
prettyFunName name = "pretty_" ++ identString name

indentFunction :: (S.Definition, S.Definition)
indentFunction =
  ( [cedecl|static void indent (unsigned); |]
  , [cedecl|
      static void indent (unsigned depth) {
        int i;
        for (i = 0; i < depth; ++i)
           printf("  ");
      } |]
  )

prettyFun :: [Decl] -> Decl -> (S.Definition, S.Definition)
prettyFun ds decl =
  ( [cedecl|void $id:(prettyFunName name)
                (unsigned, $ty:curT); |]
  , [cedecl|
    void $id:(prettyFunName name)
         (unsigned depth, $ty:curT cur) {
              $stms:(prettyFunBody env (dType decl))
    } |]
  )

  where
  curT = [cty| typename $id:(identString name) * |]
  env  = simpleEnv ds
  name = dName decl

prettyFunBody :: Env -> Type -> [S.Stm]
prettyFunBody env ty =
  case ty of

    PrimType str -> ppValue fmtStr [cexp| *cur |]
      where
      fmtStr = case str of
        "signed char"        -> "%hhd"
        "char"               -> "%c"
        "int"                -> "%d"
        "long"               -> "%ld"
        "long long"          -> "%lld"
        "short"              -> "%hd"
        "unsigned char"      -> "%hhu"
        "unsigned int"       -> "%u"
        "unsigned long"      -> "%lu"
        "unsigned long long" -> "%llu"
        "unsigned short"     -> "%hu"
        _                    -> error ("unsupported type: " ++ str)


    String        -> ppValue "%s" [cexp| cur |]

    Pointer ty'   ->
      [[cstm|
          if (*cur == NULL) {
            $stms:(rawPrint "<null>" [] (not (simple env ty')) True)
          } else {
            void *cur1 = *cur;
            $ty:(cPtrType ty') *cur = cur1;
            $stms:(prettyFunBody env ty')
          }
      |]]

    Reference ty' ->
      [[cstm| { void *cur1 = *cur;
                $ty:(cPtrType ty') *cur = cur1;
                $stms:(prettyFunBody env ty')
              }
        |]]

    Named ident  ->
      [ [cstm| $id:(prettyFunName ident)(depth, cur); |] ]

    Array ty' sz ->
     [ [cstm|
       { unsigned i;
         for (i = 0; i < $(arrSize sz); ++i) {
           $stms:(ppLabel env "[%u]:" ivar ty')
           { unsigned depth1 = 1 + depth;
             unsigned depth = depth1;
             void *cur1 = cur + i;
             $ty:(cType ty') *cur = cur1;
             $stms:(prettyFunBody env ty')
           }
         }
       }
       |]
     ]
     where ivar = [cexp| i |]

    Struct xs ->
      case xs of
        []  -> ppValue "%s" [cexp| "<empty>"|]
        _   -> [[cstm| { $items:(concatMap (structField env) xs) } |]]

    Union tag xs ->
      [[cstm| switch(cur->tag) { $stms:(map (unionField tag env) xs) } |]]


structField :: Env -> Field -> [S.BlockItem]
structField env f = sizeDecl ++ map S.BlockStm (ppThis ++ [ppSub])
  where
  ppThis  = ppLabel env "%s:" [cexp| $string:fldS|] fty
  ppSub   = [cstm| { unsigned depth1 = 1 + depth;
                     unsigned depth = depth1;
                     void *cur1 = &cur->$id:fldS;
                     $ty:(cType fty) *cur = cur1;
                     $stms:(prettyFunBody env fty)
                   }
            |]

  fty     = fType f
  fldI    = fName f
  fldS    = identString fldI

  sizeDecl
    | isIndexType env fty = map S.BlockDecl
        [[cdecl| typename size_t $id:(sizeAttrName fldI) = cur->$id:fldS; |]]
    | otherwise = []

unionField :: String -> Env -> Field -> S.Stm
unionField tag env f =
  [cstm| case $id:(enumLabel tag f):
           { $stms:(ppLabel env "case %s:" labE fty)
             { unsigned depth1 = 1 + depth;
               unsigned depth = depth1;
               void *cur1 = &cur->u;
               $ty:(cType fty) *cur = cur1;
               $stms:(prettyFunBody env fty)
             }
             break;
           }
  |]
  where
  fty   = fType f
  labE  = [cexp| $string:(identString (fName f)) |]

--------------------------------------------------------------------------------

ppLabel :: Env -> String -> S.Exp -> Type -> [S.Stm]
ppLabel env fmt arg ty = rawPrint fmt [arg] True (not (simple env ty))

ppValue :: String -> S.Exp -> [S.Stm]
ppValue fmt arg = rawPrint fmt [arg] False True


rawPrint :: String -> [S.Exp] -> Bool -> Bool -> [S.Stm]
rawPrint fmt args indent nl =
  [ [cstm| indent($indentAmt); |]
  , [cstm| printf($string:(fmt ++ terminal), $args:args); |]
  ]
  where
  indentAmt | indent    = [cexp| depth |]
            | otherwise = [cexp|1|]

  terminal  | nl        = "\n"
            | otherwise = ""



{- Simple values are displayed on a single line,
while complex line a re displayed on a new line, and indented
a bit further in. -}
simple :: Env -> Type -> Bool
simple _   (PrimType {}) = True
simple env (Pointer ty) = simple env ty
simple env (Reference ty) = simple env ty
simple env (Named ident)= simple env (dType (getDecl env ident))
simple _   String       = True
simple _   (Struct xs)  = null xs
simple _   (Union _ _)  = False
simple _   (Array {} )  = False

