import DR.Constructors
import DR.Environment  
import DR.Interpreter
import DR.ThreeLevels 

import qualified Data.Map.Strict as M


{-
Example 1) avg-attack

Variables h1, h2, h3 store the salaries of the three employees.
Variable avg is intendend to intentionally release the average but no other information
about h1, h2 nd h3.


h1 := 1; h2 := h1; h3 := h1;
avg := declassify((h1 + · · · + hn)/n, low)


Γ = {avg : Low , h1 : High , h2 : High, h3 : High}
	
-}

------------------------------------------
-- Security environment for this example
------------------------------------------

-- env = (zero, L) :-: (one, H) :-: (two, H) :-: (three, H) :-: (four, H) :-: Nil

-- Variables
(avg', env1) = newVar L initEnv 
(h1' , env2) = newVar H env1  
(h2' , env3) = newVar H env2
(h3 , env) = newVar H env3

-- actualize evironmet in variables
avg = updateEnv env avg' 
h1 = updateEnv env h1'
h2 = updateEnv env h2'

-----------------------
-- Secure program
-----------------------

avgSalaries =  avg =: declassify ((h1 +. h2 +. h3) //. int 3) L

-- Evaluation

-- Initial Memory.
memory = M.insert 0 2 (M.insert 1 10 (M.insert 2 3 (M.insert 3 2 initMemory)))

res = eval avgSalaries memory --  result:  fromList [(0,5),(1,10),(2,3),(3,2)]

-------------------------
-- Unsecure program
-------------------------

-- This program is rejected by GHC

{-
unsecureProgram = h2 =: h1 >>>
                  h3 =: h1 >>>
                  avgSalaries 

-}



