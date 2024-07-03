import AbstractSyntax
import Constructors
import Environment
import Interpreter
import TwoLevels

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

-- variable declarations
(h1 , env1) = newvar H initEnv
(l1 , env2) = newVar L env1
(k1, env3) = newVar L env2
(n, env) =  newVar L env3

-- actualice variables with the last environment
h = updateEnv env h1 
l = updateEnv env l1 
k = updateEnv env k1

------------------------------------------
-- Unsecure Program
------------------------------------------
secureWallet = iff (declassify (h  >=. k) L)
                   (h =: h -. k  >>> l =: l +. k)
                   skip


-- This program is rejected by Haskell type checker

attack  = l =: int 0 >>> 
          while (n >=. int 0)  
                ((k =:  int 2 ^. n -. int 1) >>> 
	         secureWallet  >>>  
	         (n =:  n -. int 1))

