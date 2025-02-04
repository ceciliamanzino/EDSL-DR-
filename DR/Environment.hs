{-# LANGUAGE DataKinds,
             TypeOperators,
             KindSignatures,
             TypeFamilies,
             GADTs #-}

module DR.Environment where

import DR.AbstractSyntax
import DR.ThreeLevels (SType, SeType)
import Data.Kind (Type)

infixr 5 :-:

-- given an environment return its size 
type family Next (xs :: [(Nat , SType)]) :: Nat where 
    Next '[] = Zero 
    Next ('(m, x) ': env) = Succ m   

-- data type used to represent the security environment 
data Env :: [(Nat , SType)] -> * where
    Nil :: Env '[] 
    (:-:) :: SeType s1 -> Env xs -> Env ('( Next xs , s1) ': xs) 


------------------------------------------------------------------------
--- Constructors for defining the security environment of variables ----
------------------------------------------------------------------------

-- Introduce an environment
--setEnv :: Env env -> Stm env 'High '[] '[]
--setEnv env = Skip

-- given an environment this function return the next natural number 
-- that will be asociated to a new variable
newId :: Env xs -> SNat (Next xs)
newId Nil = SZero 
newId (s :-: env) = SSucc (newId env) 


-- given a security type and the actual security environment
-- this function return a new variable associted with the given securirty type
-- and the updated environment
newVar ::   SeType (l :: SType)      -- security type of the new variable 
         -> Env  xs -- security environment
         -> (Exp ('( Next xs , l) ': xs) l '[] '[ Next xs ] , 
             Env ('( Next xs , l) ': xs))
newVar st env  = (Var (newId env) , st :-: env )

  
-- Every time a variable is added to the environment, 
-- the environment of the variables that were previously defined must be updated
-- This function receives the new environment and an old variable
-- and returns the same variable, but with the environmet updated in its type
updateEnv :: (Lookup env n) ~ (Lookup env' n) =>
          Env env' ->  
          Exp env (Lookup env n) '[] '[n] ->           
          Exp env' (Lookup env' n) '[] '[n]
updateEnv env (Var n) = Var n    

initEnv = Nil

-------------------------------------------------------------
--------- Example of use  -----------------------------------
-------------------------------------------------------------


-- Here we show with an example the declaration of the following variables: {h : High, l: Low, m : Medium} 

{- 
-- We declare the variables with their security levels
(h1 , env1) = newVar H initEnv
(m1 , env2) = newVar M env1
(l, env) = newVar L env2


-- we update the variables h and m with the final environment
h = updateEnv env h1
m = updateEnv env m1
-}





