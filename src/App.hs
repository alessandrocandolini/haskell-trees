{-# LANGUAGE DerivingVia #-}
module App where
import Data.Text as T
import Control.Monad.State


program :: IO ()
program = putStrLn "hello!"

-- Update to non empty
data Tree a = Leaf a | Node a [Tree a] deriving (Eq, Show)

one:: Tree Int
one = Leaf 1

basic :: Tree Int
basic = Node 1 [Leaf 2, Leaf 3]

more :: Tree Int
more = Node 1 [ Node 2 [Leaf 3, Leaf 4], Node 5 [Leaf 6, Node 7 [Leaf 8, Leaf 9, Leaf 10]]]

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
