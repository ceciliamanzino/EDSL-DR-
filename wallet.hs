import DR.Constructors
import DR.Environment  
import DR.Interpreter
import DR.ThreeLevels    

import qualified Data.Map.Strict as M

{-
Example 2) Electronic Wallet

- h stores the (secret) amount of money in a customers electronic wallet
- l stores the (public) amount of money spent during the current session  
- k stores the cost of the item to be purchased 


Γ = {h : High, l : Low, k : Low }

Secure program:

if  declassify(h >= k, low) then (h := h - k;l := l + k) else Skip

-}

------------------------------------------
-- Security environment for this example
------------------------------------------

-- env = (zero, H) :-: (one, L) :-: (two, L) :-: Nil

-- variables 
(h', env1) = newVar H initEnv
(l', env2) = newVar L env1
(k , env) = newVar L env2

-- actualize evironmet in variables
h = updateEnv env h' 
l = updateEnv env l'

secureWallet = iff (declassify (h  >. k) L)
                   (h =: h -. k  >>> l =: l +. k)
                   skip



-- Testing with a different environment.
memory =  M.insert 0 500 (M.insert 1 0 (M.insert 2 45 initMemory))


main = eval secureWallet memory -- fromList [(0,455),(1,45),(2,45)]


