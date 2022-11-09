module Examples where 

import Tree

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


