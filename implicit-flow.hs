import DR.AbstractSyntax
import DR.Constructors
import DR.Environment
import DR.Interpreter

import qualified Data.Map.Strict as M


-- Example 4) Implicit Flow
-- if xH then yL := 1 else skip

-------------------------------------------
-- Security environment for this example
-------------------------------------------

env = (zero, H) :-: (one, L) :-: Nil

-- variables
xH = var env zero
yL = var env one

---------------------------------
-- Unsecure Program
---------------------------------

-- program1  =  iff xH (one =: int 1) skip











