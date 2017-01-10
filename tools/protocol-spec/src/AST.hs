module AST where

import Data.Hash
import Data.List

newtype Ident = I String deriving (Eq,Show,Ord)

identString :: Ident -> String
identString (I x) = x

data Spec = Spec
  { codecs    :: [ Codec ]    -- ^ Generate these codec functions
  , typeDecls :: [ Decl ]   -- ^ The known formats.
  , hashes    :: [ (Ident, Hash) ]
  }
  deriving (Show)

data Codec = Codec
  { encodeName :: Ident
  , decodeName :: Ident
  , codecType  :: Ident
  }
  deriving (Show)

data Decl = Decl
  { dName   :: Ident
  , dType   :: Type
  }
  deriving (Show)

data Type
  = PrimType String
  | Pointer Type              -- ^ A pointer to something
  | Reference Type            -- ^ Like a pointer but non-NULLABLE
  | Named Ident

  -- Note that the next 4 fields are NOT pointers.
  | String                    -- ^ A 0-terminated sequence.
  | Struct [ Field ]          -- ^ Adjacent fields.
  | Union String [ Field ]
  | Array Type Size
  deriving (Show)

data Size
  = FixedSize Int
  | VarSize Ident      -- could be a more complex expression
  deriving (Show)

data Field = Field
  { fName :: Ident
  , fType :: Type
  }
  deriving (Show)

instance Hashable Ident where hash (I a) = hash a
instance Hashable Size  where
  hash e = hash $ case e of
                    FixedSize x -> Left x
                    VarSize   x -> Right x


hashType :: [Decl] -> Type -> Hash
hashType types t
  = case t of
      PrimType str -> tag 0 $ hash str
      Pointer  ty  -> tag 1 $ hashType types ty
      Reference ty -> tag 2 $ hashType types ty
      Named name   -> let Just d = find (\x -> dName x == name) types
                   in tag 3 $ hashType types (dType d)
      String       -> hashWord8 4
      Struct xs    -> foldl' combine (hashWord8 5) $ map (hashField types) xs
      Union str xs -> foldl' combine (hashWord8 6 `combine` hash str)
                          $ map (hashField types) xs
      Array ty sz  -> tag 7 $ hash sz `combine` hashType types ty
  where
  tag n x = hashWord8 n `combine` x

hashField :: [Decl] -> Field -> Hash
hashField types f = hash (fName f) `combine` hashType types (fType f)
