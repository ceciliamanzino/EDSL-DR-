import AbstractSyntax
import Constructors
import Environment
import Interpreter
import TwoLevels

import qualified Data.Map.Strict as M

-------------------------------
-- Security environment
----------------------------
-- buildHash applies Cantor pairing function to the password and userId given as input to build the hash.

cantor k1 k2 = ((k1 +. k2)  *. (k1 +. k2 +. int 1)) //. int 2  +. k2 

-- hash applies buildHash to the password and userId given as input

hash pwd id = declassify (cantor pwd id) L 

-- variable declaration 
(newPwd', env1) = newVar H initEnv
(oldPwd', env2) = newVar H env1
(userId', env3) = newVar L env2
(pwdIdHash, env) = newVar L env3

-- variables
newPwd = updateEnv env newPwd'
oldPwd = updateEnv env oldPwd'  
userId = updateEnv env userId'


updatePwd  =  iff (pwdIdHash  =. hash oldPwd userId)
                  (pwdIdHash =: hash newPwd userId) 
                  skip








