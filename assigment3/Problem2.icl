module serialize3Start

/*
  Definitions for assignment 3 in AFP 2016
  Kind indexed gennerics
  Pieter Koopman, pieter@cs.ru.nl
  September 2016
*/

import StdEnv, StdMaybe, StdGeneric

//------------
// gWrite
//-------------
generic gWrite a :: a [String] -> [String]
gWrite{|Int|}         a b = [(toString a):b]
gWrite{|Bool|}        a b = [(toString a):b]

//---- Generic

gWrite{|UNIT|}        a b 								= b
gWrite{|PAIR|}   f g (PAIR x y) c 						= f x (g y c)
gWrite{|EITHER|} f g (LEFT x)   c 						= (f x c)
gWrite{|EITHER|} f g (RIGHT x)  c 						= (g x c)
gWrite{|CONS of {gcd_arity,gcd_name}|}   f   (CONS x)   c 	| gcd_arity > 0
															= ["(":f x [")":c]]
															= [gcd_name:f x c]
														
gWrite{|OBJECT of {gtd_name}|} f   (OBJECT x) c 			= f x c


//------------
// gRead
//-------------
generic gRead a :: [String] -> Maybe (a, [String])
gRead{|Int|}         [s:r] 
							# i = toInt s
    						| s == toString i
      						= Just (i,r)
      						= Nothing
      						
gRead{|Bool|} ["True":r] 	= Just(True, r)
gRead{|Bool|} ["False":r]	= Just(False, r)
gRead{|Bool|} r				= Nothing

//---- Generic

gRead{|UNIT|}   r 		= Just(UNIT, r)

gRead{|PAIR|}   f g r 	= case f r of
							Just (a, m) = case g m of
								Just (b, n) = Just (PAIR a b, n)
								_ = Nothing
							_ = Nothing
								
gRead{|EITHER|} f g r   = case f r of
							Just (a,m) = Just (LEFT a,m)
							_ = case g r of
								Just (b,m) = Just (RIGHT b,m)
								_ = Nothing
								
gRead{|CONS of {gcd_arity, gcd_name}|} f   [x:l] 	| gcd_arity > 0 
												= case x of
													"(" 	= case f l of
																Just (a,[")":m]) 	= Just (CONS a, m)
																_ 					= Nothing
													_			= Nothing
												= case (x == gcd_name) of
													True = case f l of 
														Just (a,m)	= Just (CONS a, m)
														_			= Nothing
													False = Nothing

gRead{|OBJECT|} f   l 	= case f l of
								Just (a,m) = Just (OBJECT a, m)
								_ = Nothing


class serialize a where
  write :: a [String] -> [String]
  read  :: [String] -> Maybe (a,[String])
  
  writecomma :: a [String] -> [String]
  readcomma  :: [String] -> Maybe (a,[String])

instance serialize Bool where
  write b c = gWrite{|*|} b c
  read a	= gRead{|*|} a
  
  writecomma b c = gWrite{|*|} b [",":c]
  readcomma a = case gRead{|*|} a of
  					Just(b, [",":m]) 	= Just(b, m)
  					_ 					= Nothing 

instance serialize Int where
  write b c = gWrite{|*|} b c
  read a	= gRead{|*|} a
  
  writecomma b c = gWrite{|*|} b [",":c]
  readcomma a = case gRead{|*|} a of
  					Just(b, [",":m]) 	= Just(b, m)
  					_ 					= Nothing 


instance serialize [a] | serialize a where					
	write l c = gWrite{|*->*|} write l c
	read l = gRead{|*->*|} read l
	
	writecomma b c = gWrite{|*->*|} write b [",":c]
	readcomma a = case gRead{|*->*|} read a of
  					Just(b, [",":m]) 	= Just(b, m)
  					_ 					= Nothing 

// ---

:: Bin a = Leaf | Bin (Bin a) a (Bin a)

instance == (Bin a) | == a where
  (==) Leaf Leaf = True
  (==) (Bin l a r) (Bin k b s) = l == k && a == b && r == s
  (==) _ _ = False


instance serialize (Bin a) | serialize a where
	write a c = gWrite{|*->*|} write a c
	read l = gRead{|*->*|} read l
	
	writecomma b c = gWrite{|*->*|} write b [",":c]
	readcomma a = case gRead{|*->*|} read a of
  					Just(b, [",":m]) 	= Just(b, m)
  					_ 					= Nothing 
// ---

:: Coin = Head | Tail

instance == Coin where
  (==) Head Head = True
  (==) Tail Tail = True
  (==) _    _    = False


instance serialize Coin where
	write c s = gWrite{|*|} c s 
	read    l = gRead{|*|} l
	
	writecomma b c = gWrite{|*|} b [",":c]
	readcomma a = case gRead{|*|} a of
  					Just(b, [",":m]) 	= Just(b, m)
  					_ 					= Nothing 


/*
	Define a special purpose version for this type that write0s and read0s
	the value (7,True) as ["(","7",",","True",")"]
*/
instance serialize (a,b) | serialize a & serialize b where
	write t c = gWrite{|*->*->*|} writecomma write t c
	read r = gRead{|*->*->*|} readcomma read r
	
	writecomma b c = gWrite{|*->*->*|} writecomma write b [",":c]
	readcomma a = case gRead{|*->*->*|} readcomma read a of
  					Just(b, [",":m]) 	= Just(b, m)
  					_ 					= Nothing
	
derive gWrite (,), [], Bin, Coin
derive gRead (,), [], Bin, Coin

// ---
// output looks nice if compiled with "Basic Values Only" for console in project options

/*
Output:
Oke, write produces: True
Oke, write produces: False
Oke, write produces: 0
Oke, write produces: 123
Oke, write produces: -36
Oke, write produces: (42_Nil)
Oke, write produces: (0(1(2(3(4_Nil)))))
Oke, write produces: ((True_Nil)_Nil)
Oke, write produces: (LeafTrueLeaf)
Oke, write produces: (((Leaf(1_Nil)Leaf)(2_Nil)(Leaf(3_Nil)(Leaf(4(5_Nil))Leaf)))_Nil)
Oke, write produces: (((Leaf(1_Nil)Leaf)(2_Nil)(Leaf(3_Nil)((Leaf(4(5_Nil))Leaf)(6(7_Nil))(Leaf(8(9_Nil))Leaf))))_Nil)
Oke, write produces: Head
Oke, write produces: Tail
Oke, write produces: (7,True)
Oke, write produces: (Head,(7,(Tail_Nil)))
End of the tests.
*/
Start = 
  [test True
  ,test False
  ,test 0
  ,test 123
  ,test -36
  ,test [42]
  ,test [0..4]
  ,test [[True]]
  ,test (Bin Leaf True Leaf)
  ,test [Bin (Bin Leaf [1] Leaf) [2] (Bin Leaf [3] (Bin Leaf [4,5] Leaf))]
  ,test [Bin (Bin Leaf [1] Leaf) [2] (Bin Leaf [3] (Bin (Bin Leaf [4,5] Leaf) [6,7] (Bin Leaf [8,9] Leaf)))]
  ,test Head
  ,test Tail
  ,test (7,True)
  ,test (Head,(7,[Tail]))
  ,["End of the tests.\n"]
  ]

test :: a -> [String] | serialize, == a
test a = 
  (if (isJust r)
    (if (fst jr == a)
      (if (isEmpty (tl (snd jr)))
        ["Oke"]
        ["Not all input is consumed! ":snd jr])
      ["Wrong result: ":write (fst jr) []])
    ["read result is Nothing"]
  ) ++ [", write produces: ": s]
  where
    s = write a ["\n"]
    r = read s
    jr = fromJust r
