module Rose where 
import Tree
import Data.List.NonEmpty

data Rose a = Rose {
  value :: a,
  children :: [Rose a] } deriving (Eq,Show)

toRose :: Tree a -> Rose a
toRose (Leaf a) = Rose a []
toRose (Node a t) = Rose a (( fmap toRose . toList) t)

fromRose :: Rose a -> Tree a
fromRose (Rose a rs)=  maybe (Leaf a) (Node a) ((nonEmpty . fmap fromRose) rs)



