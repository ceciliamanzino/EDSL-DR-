{-# LANGUAGE DataKinds,
             GADTs, 
             TypeFamilies,
             PolyKinds, 
             TypeOperators,
             MultiParamTypeClasses, 
             FlexibleInstances,
             FlexibleContexts,
             UndecidableInstances #-}

import DR.Constructors
import DR.Environment  
import DR.Interpreter
import DR.ThreeLevels 

import qualified Data.Map.Strict as M

{-
Example 3) Password checker attack.

l := 0;
while (n = 0) do
   k := 2n-1;
   if hash(sign(h - k + 1), 0) = hash(1, 0)
    then (h := h - k; l := l + k) 
    else skip;
  n := n - 1
-}


-- Database 
memory =  M.insert 0 23 (M.insert 1 45 (M.insert 2 23391 (M.insert 3 500 (M.insert 4 0 initMemory))))

---------------------------------------
-- Security environment
--------------------------------------

-- variable declaration 
(l', env1) = newVar L initEnv
(h', env2) = newVar H env1
(k', env3) = newVar L env2
(n , env) = newVar L env3

-- variables 
l = updateEnv env l'
h = updateEnv env h'
k = updateEnv env k'

cantor k1 k2 = ((k1 +. k2)  *. (k1 +. k2 +. int 1)) //. int 2  +. k2 
         
hash pwd id = declassify (cantor pwd id) L 


----------------------------------
--- Unsecure Program
---------------------------------
      
signo e = toUnOp sgn e
            where sgn x | x == 0 = 0
                        | x > 0  = 1
                        | x < 0  = -1


-- this code is rejected by GHC
{-
pwdAttack = l =: int 0 >>>
            (while (n >=. int 0)
                   ( k =:  int 2 ^. (n -. int 1)  >>> 
	             (iff (hash (signo (h -. k +. (int 1))) (int 0)  =. hash (int 1) (int 0)) 
	                  (h =: h -. k  >>> l =: l +. k) 
	                  skip) >>>
	             n =:  n -. int 1))

-}








