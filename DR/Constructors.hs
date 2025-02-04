{-# LANGUAGE DataKinds,
             GADTs, 
             TypeFamilies,
             PolyKinds, 
             TypeOperators,
             MultiParamTypeClasses, 
             FlexibleInstances,
             FlexibleContexts,
             UndecidableInstances #-}

module DR.Constructors where

import DR.AbstractSyntax
import DR.Environment
import Control.Monad.State 
import qualified Data.Map.Strict as M
import Data.Maybe
import DR.ThreeLevels

----------------------------
--- Expression constructors
----------------------------

-- Integer literals
infixr 6 `int`, `bool`
int :: Int -> Exp env 'Low '[] '[]
int = IntLit

num :: Exp env 'Low '[] '[] -> Int
num (IntLit n) = n

-- Boolean literals
bool :: Bool -> Exp env 'Low '[] '[]
bool = BoolLit

-- Operators
infixr 6 +., -.
infixr 7 *., //., %.

infixr 5 >., >=., <., <=.
infixr 4 =., \=. 
infixr 3 &&.
infixr 2 ||.

-- boolean operators
neg = OpUn (UnBool not) 
(&&.) = OpBin (BinBool (&&))  
(||.) = OpBin (BinBool (||))   

-- integer operators
(+.)  = OpBin (BinInt (+))
(-.)  = OpBin (BinInt (-)) 
(*.)  = OpBin (BinInt (*)) 
(//.) = OpBin (BinInt div)  
(%.)  = OpBin (BinInt mod)

-- relational operators  
(^.)  = OpBin (BinInt (^)) 
(>.)  = OpBin (PredInt (>)) 
(>=.) = OpBin (PredInt (>=)) 
(<.)  = OpBin (PredInt (<)) 
(<=.) = OpBin (PredInt (<=)) 
(=.)  = OpBin (PredInt (==)) 
(\=.) = OpBin (PredInt (/=))  

-- this constructor transforms a binary function on integers into a function
-- over expressions
toBinOp :: (Int -> Int -> Int) -> Exp env st d var1  -> Exp env st' d' var2  -> 
          Exp env (Join st st') (Union d d') (Union var1 var2) 
toBinOp f =  OpBin (BinInt f) 

-- this constructor transforms an unary function on integers into a function
-- over expressions
toUnOp :: (Int -> Int) -> Exp env st d var -> Exp env st d var
toUnOp f = OpUn (UnInt f) 

----------------------------
--- Statement constructors
----------------------------
          
-- Assigment
infixr 2 =:
(=:) :: LEq st (Lookup env n) => 
           Exp env (Lookup env n) '[] '[n] 
        -> Exp env st d var 
        -> Stm env (Lookup env n) '[n] d 
(=:) (Var n) exp = Ass n exp


-- The expression declassify
declassify :: Exp env l' d vars -> SeType l -> Exp env l vars vars
declassify e l = Declassify e l 

-- Term sequence
infixr 1 >>>
(>>>) :: (Intersection u1 d2 ~ '[]) =>
          Stm env pc u1 d1 -> 
          Stm env pc' u2 d2 -> 
          Stm env (Meet pc pc') (Union u1 u2) (Union d1 d2)
(>>>) c1 c2 = Seq c1 c2

-- Skip
skip = Skip

-- Conditional
infixr 6 `iff`
iff e c1 c2 = If e c1 c2

-- Loop
infixr 6 `while`
while e c = While e c

-- Output
infixr 6 `output`
output e = Output e

-- for
infixr 6 `for`
for c1 e2 c3 c = c1 >>> while e2 (c >>> c3)     



