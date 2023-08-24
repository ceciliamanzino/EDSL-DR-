import DR.AbstractSyntax
import DR.Constructors
import DR.Environment
import DR.Interpreter

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

env = (zero, L) :-: (one, H) :-: (two, H) :-: (three, H) :-: (four, H) :-: Nil

-- Variables
avg = var env zero
h1 = var env one  
h2 = var env two 
h3 = var env three 

-----------------------
-- Secure program
-----------------------

avgSalaries = zero =: declassify ((h1 +. h2 +. h3) //. int 3) L

-- Evaluation

-- Initial Memory.
memory = M.insert 0 2 (M.insert 1 10 (M.insert 2 3 (M.insert 3 2 initMemory)))

res = eval avgSalaries memory --  result:  fromList [(0,5),(1,10),(2,3),(3,2)]

-------------------------
-- Unsecure program
-------------------------

-- This program is rejected by the Haskell type checker

{-
unsecureProgram = one =: int 100 >>>   -- Swapping the values h1, h2.
                  two =: h1 >>>
                  three =: h1 >>>
                  avgSalaries 
-}




