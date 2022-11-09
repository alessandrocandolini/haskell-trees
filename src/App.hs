{-# LANGUAGE DerivingVia #-}
module App where
import Data.Text as T
import Control.Monad.State
import Data.List.NonEmpty


program :: IO ()
program = putStrLn "hello!"

-- non-empty Rose tree of arbitrary order, lazy, possibly infinite
data Tree a = Leaf a | Node a (NonEmpty (Tree a)) deriving (Eq, Show)



separator :: String
separator = "-"

newtype Depth = Depth Int deriving (Eq,Show)
   deriving Num via Int

newtype Label a = Label a deriving (Eq,Show)


labelling :: Tree a -> State Depth (Tree (Label a))
labelling (Leaf a) = pure $ Leaf (Label a)
labelling (Node a bs) = undefined


pretty :: Show a => Tree a -> Text
pretty (Leaf a) = pack $ show a
pretty (Node a ( Leaf a2 :| [] )) = pack ( show a ++ "\n" ++ "-" ++ show a2)
pretty (Node a _) = "???"
