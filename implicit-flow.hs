import DR.Constructors
import DR.Environment  
import DR.Interpreter
import DR.ThreeLevels 

import qualified Data.Map.Strict as M

-------------------------------------------
-- Security environment for this example
-------------------------------------------

-- variables 
(h', env1) = newVar H initEnv
(l, env) = newVar L env1

-- actualize evironmet in variables
h = updateEnv env h'

---------------------------------
-- Unsecure Programs
---------------------------------

{-
unsecure1  =  iff h (l =: int 1) skip

unsecure2 = while (l <. h) (l =: l +. (int 1))
-}









