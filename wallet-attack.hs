import DR.AbstractSyntax
import DR.Constructors
import DR.Environment
import DR.Interpreter

import qualified Data.Map.Strict as M

{-
Example 2) Electronic Wallet 

- h stores the (secret) amount of money in a customers electronic wallet
- l stores the (public) amount of money spent during the current session   
- k stores the cost of the item to be purchased 


Γ = {h : High, l : Low, k : Low, n : Low}

Unsecure program: 

l := 0; 
while (n ≥ 0) do
  k := (2 exp n) − 1;
  if declassify(h >= k, low) then (h := h − k; l := l + k) else skip;
  n := n − 1
-}

------------------------------------------
-- Security environment for this example
-----------------------------------------

env = (zero, H) :-: (one, L) :-: (two, L) :-: (three, L) :-: (four, L)  :-: Nil


-- variables 
h = var env zero
l = var env one
k = var env two
n = var env three

------------------------------------------
-- Unsecure Program
------------------------------------------


-- This program is rejected by Haskell type checker

attack  = one =: int 0 >>> 
          while (n >. int 0)  
                (two =:  int 2 ^. n -. int 1) >>> 
	  (iff (declassify (h >=. k) L) 
	       (zero =:  h -. k >>> one =:  l +. k) 
	       skip) >>>  
	  (three =:  n -. int 1)



          

      


