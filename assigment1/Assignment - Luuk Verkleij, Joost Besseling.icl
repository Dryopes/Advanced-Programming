/*
* Gemaakt door:
*	Luuk Verkleij (s4662261)
*	Joost Bessing (s4796799)
*/

module container

import StdEnv
import StdMaybe

/*
2. Kinds
:: Bin a = Leaf | Bin (Bin a) a (Bin a)
* -> *

:: Tree a b = Tip a | Node (Tree a b) b (Tree a b)
* -> * -> *

:: Rose a = Rose a [Rose a]
* -> *

:: T1 a b = C11 (a b) | C12 b
(* -> *) -> * -> *

:: T2 a b c = C2 (a (T1 b c))
(* -> *) -> (* -> *) -> * -> *

:: T3 a b c = C3 (a b c)
(* -> * -> *) -> * -> * -> *

:: T4 a b c = C4 (a (b c))
(* -> *) -> (* -> *) -> * -> *

*/

:: Bin a = Leaf | Bin (Bin a) a (Bin a)
:: Rose a = Rose a [Rose a]

concat :: [[a]] -> [a]
concat as = foldr (++) [] as

class serialize a where
	write :: a [String] -> [String]
	read :: [String] -> Maybe (a,[String])
	
instance serialize Int where
	write b c = [toString b:c]
	read [x:r] = if (toInt x == 0 && x <> "0") Nothing (Just(toInt x, r))
	read _ = Nothing

instance serialize [a] | serialize a where
	write bs c = ["startlist"] ++ (concat (map (flip write []) bs)) ++ ["endlist"]
	
	read ["startlist":xs] = read_r xs
		where	read_r ["endlist":xs] 	= Just([], xs)
				read_r xs				= case readit of 
											Nothing -> Nothing
											_ 		-> case next of 
														Nothing -> Nothing
														_		-> Just([readit_fst] ++ next_fst, next_snd)
					where	readit = read xs
							readit_fst = fst (fromJust readit)
							readit_snd = snd (fromJust readit)
							next = read_r readit_snd
							next_fst = fst (fromJust next)
							next_snd = snd (fromJust next)					
	read [] = Nothing

instance serialize (Rose a) | serialize a where
	write (Rose v rs) c = ["startrose"] ++ (write v []) ++ (write rs []) ++ c
	read ["startrose":xs] = Just((Rose readvalue_fst readarr_fst), readarr_snd)
						where 	readvalue = read xs
								readvalue_fst = fst (fromJust readvalue)
								readvalue_snd = snd (fromJust readvalue)
								readarr = read readvalue_snd
								readarr_fst = fst (fromJust readarr)
								readarr_snd = snd (fromJust readarr)
								
	read _ = Nothing



class Container t where
    Cinsert   :: a (t a) -> t a      | <        a
    Ccontains :: a (t a) -> Bool     | <, Eq    a
    Cshow     ::   (t a) -> [String] | serialize a
    Cnew      :: t a
    
instance Container [] where
	Cinsert a xs = [a:xs]
	Ccontains a xs =  length (filter (\z -> a==z) xs) > 0 
	Cshow xs = write xs []
	Cnew = []
	
instance Container Bin where
	Cinsert x (Bin a b c) = if (b<x) (Bin (Cinsert x a) b c) (Bin a b (Cinsert x c))
	Cinsert x Leaf = (Bin Leaf x Leaf)
	Ccontains x Leaf = False
	Ccontains x (Bin a b c) = b == x || if (b < x) (Ccontains x a) (Ccontains x c)
	Cshow _ = ["groetjes"] // write xs []
	Cnew = Leaf
	
first :: (Rose Int, b) -> Rose Int
first (a,_) = a

second :: (a,b) -> b
second (a,b) = b

	
Start = first (fromJust rose)
	where rose = read (write (Rose 2 [Rose 3 [], Rose 4 [Rose 12 []], Rose 7 []]) [])
