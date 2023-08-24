import DR.AbstractSyntax
import DR.Constructors
import DR.Environment
import DR.Interpreter

import qualified Data.Map.Strict as M

{-
Example 3) Password checker

-}
-------------------------------
-- Security environment
----------------------------

env = (zero, H) :-: (one, L) :-: (two, H) :-: (three, H) :-: (four, H) :-: Nil 

-- variables
pwd = var env zero
userId = var env one
pwdUserIdHash = var env two
tmp = var env three
newPwd = var env four

-----------------------------------------
---- Secure Program
-----------------------------------------


-- buildHash applies Cantor pairing function to the password and userId given as input to build the hash.
         
buildHash = ((pwd +. userId)  *. (pwd +. userId +. int 1)) //. int 2  +. userId

-- hash applies buildHash to the password and userId given as input

hash = declassify buildHash L 


-- match checks if the password image, or hash, from the database is equal to the hash of 
-- the user input

match = three =: (pwdUserIdHash  =. hash)

-- Database 
database = M.insert 0 23 (M.insert 1 45 (M.insert 2 2391 (M.insert 3 0 (M.insert 4 67 initMemory))))


test1 = eval match database -- fromList [(0,23),(1,45),(2,2391),(3,1),(4,67)]


{-

updatePwd updates the old password hash 
by querying the old password, matching its hash and (if
matched) updating the hashed password with the hash of the password.
          
-}

updatePwd = iff (pwdUserIdHash  =. hash)
                (two =: declassify (((newPwd +. userId)  *. (newPwd +. userId +. int 1)) //. int 2  +. userId) L) 
                skip


test2 = eval updatePwd database -- fromList [(0,23),(1,45),(2,6373),(3,0),(4,67)]







