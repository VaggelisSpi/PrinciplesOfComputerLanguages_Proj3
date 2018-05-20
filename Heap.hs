module Heap (Heap(Heap, Nil), mix, top, pop, merge, heapFromList, isEmpty) where 

data Heap a = Nil | Heap !Int a [Heap a] deriving (Show)

mix:: Heap a-> Heap a-> Heap a
mix Nil y@(Heap j b bs) = y
mix y@(Heap j b bs) Nil = y
mix x@(Heap i a as) y@(Heap j b bs)
    |i<=j = Heap i a (y:as)
    |otherwise = Heap j b (x:bs)

isEmpty::Heap a -> Bool
isEmpty Nil = True
isEmpty x = False

top:: Heap a->(Int, a)
top (Heap i a _) = (i,a)

pop:: Heap a ->Heap a
pop (Heap _ _ []) = Nil
pop (Heap _ _ (x:xs)) = merge x xs

merge:: Heap a->[Heap a]->Heap a
merge x (y:ys) =case ys of
    (z:zs)-> mix x y `mix` merge z zs
    [] -> mix x y
merge x [] = x


heapFromList::[Int]->[a]->(Heap a)->(Heap a)
heapFromList [x] [y] h = mix h (Heap x y [])
heapFromList (x:xs) (y:ys) h = (heapFromList xs ys new_h)
    where new_h = mix h (Heap x y [])


tuppleHeapFromList::[Int]->[a]->[b]->(Heap (a,b))->(Heap (a,b))
tuppleHeapFromList [x] [y] [z] h = mix h (Heap x (y,z) [])
tuppleHeapFromList (x:xs) (y:ys) (z:zs) h = mix new_h (tuppleHeapFromList xs ys zs new_h)
       where new_h = mix h (Heap x (y,z) []) 

