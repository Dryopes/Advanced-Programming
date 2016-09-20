module genericMap

import StdEnv, StdGeneric

generic gMap a b :: a -> b
gMap{|Int|}         x = x
gMap{|Real|}        x = x
gMap{|UNIT|}        x = x
gMap{|PAIR|}   f g (PAIR x y) = PAIR   (f x) (g y) 
gMap{|EITHER|} f g (LEFT x)   = LEFT   (f x)
gMap{|EITHER|} f g (RIGHT x)  = RIGHT  (g x)
gMap{|CONS|}   f   (CONS x)   = CONS   (f x)
gMap{|OBJECT|} f   (OBJECT x) = OBJECT (f x)

:: Bin a = Leaf | Bin (Bin a) a (Bin a)
t = Bin (Bin Leaf 1 Leaf) 2 (Bin (Bin Leaf 3 Leaf) 4 Leaf)
l = [1..7]

derive gMap Bin
derive gMap []
derive gMap (,)

fac :: Int -> Int
fac 0 = 1
fac a = a * fac(a-1)

// This is the type the compiler gives, but we can't use it manually?
//facmap :: (a Int) -> a Int | gMap_ss a
facmap x = gMap{|*->*|} fac x


//Output: (Bin (Bin Leaf 1 Leaf) 2 (Bin (Bin Leaf 6 Leaf) 24 Leaf))
//Start = gMap{|*->*|} fac t

//Output: [(1,1),(2,2),(3,6),(4,24),(5,120),(6,720),(7,5040)]
//Start = gMap{|*->*|} (\x -> (x, fac x)) l

//Output: ([1,2,6,24,120,720,5040],(Bin (Bin Leaf 1 Leaf) 2 (Bin (Bin Leaf 6 Leaf) 24 Leaf)))
Start = gMap{|*->*->*|} facmap facmap (l, t)

	
