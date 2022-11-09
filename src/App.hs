{-# LANGUAGE DerivingVia #-}
module App where
import Data.Text as T
import Control.Monad.State
import Data.List.NonEmpty


program :: IO ()
program = putStrLn "hello!"

-- non-empty Rose tree of arbitrary order, lazy, possibly infinite
data Tree a = Leaf a | Node a (NonEmpty (Tree a)) deriving (Eq, Show)

data Rose a = Rose {
  value :: a,
  children :: [Rose a] } deriving (Eq,Show)


toRose :: Tree a -> Rose a
toRose (Leaf a) = Rose a []
toRose (Node a t) = Rose a (( fmap toRose . toList) t)

fromRose :: Rose a -> Tree a
fromRose (Rose a []) = Leaf a
fromRose (Rose a (h : t)) = Node a (fromRose h :| fmap fromRose t)


one:: Tree Int
one = Leaf 1

basic1 :: Tree Int
basic1 = Node 1 [Leaf 2]

basic2 :: Tree Int
basic2 = Node 1 [Node 2 [Leaf 3]]

basic3 :: Tree Int
basic3 = Node 1 [Leaf 2, Leaf 3]

more :: Tree Int
more = Node 1 [Node 2 [Leaf 3, Leaf 4], Node 5 [Leaf 6, Node 7 [Leaf 8, Leaf 9, Leaf 10]]]

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
pretty (Node a _) = pack $ show a
