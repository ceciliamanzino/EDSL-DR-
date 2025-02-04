import DR.Constructors
import DR.Environment  
import DR.Interpreter
import DR.ThreeLevels 

import qualified Data.Map.Strict as M

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

-- environments of values
memory = M.insert 0 30 (M.insert 1 10 (M.insert 2 10 initMemory))
memory' = M.insert 0 10 (M.insert 1 10 (M.insert 2 10 initMemory))


secure1 = for (l =: int 0) (l <. int 10) (l =: l +. int 2) (output l)


-- is not typed
{-
unsecure =  l =: int 0 >>> 
            while (l  <. h)
                  (output l)
-}


-- it is typed but it has a non-terminating loop (so, no outputs are shown) 
secureNonterminated = l =: int 0 >>>
                       while (l <. int 20)
                           (output l  >>> l =: l +. (int 1) >>> iff (l >. h) (while (bool True) skip) skip )
 
evalSecure1 = eval secure1 memory

-- no output are shown because the loop terminate
evalSecureNonterminated = eval secureNonterminated memory' 

-- the outputs are shown because the loop terminates
evalSecureTerminated = eval secureNonterminated memory



