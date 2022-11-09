{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}
module Tree where 

import Data.Text as T ( Text, pack )
import Control.Monad.State ( State )
import Data.List.NonEmpty ( NonEmpty(..), toList )
import GHC.Generics ( Generic )
import Text.PrettyPrint.GenericPretty ( Out(doc, docList) )


-- non-empty Rose tree of arbitrary order, lazy, possibly infinite
data Tree a = Leaf a | Node a (NonEmpty (Tree a)) deriving (Eq, Show, Generic)

instance (Out a) => Out (Tree a)

instance (Out a) => Out (NonEmpty a) where
  doc = docList . toList



separator :: Depth -> String
separator (Depth 0) = "|--"
separator _ = "--"



newtype Depth = Depth Int deriving (Eq,Show)
   deriving Num via Int

newtype Label a = Label a deriving (Eq,Show)


labelling :: Tree a -> State Depth (Tree (Label a))
labelling (Leaf a) = pure $ Leaf (Label a)
labelling (Node a bs) = undefined


pretty :: Show a => Tree a -> Text
pretty (Leaf a) = pack $ show a
pretty (Node a ( Leaf a2 :| [] )) = pack ( show a ++ "\n" ++ separator (Depth 0) ++ show a2)
pretty (Node a _) = "???"
