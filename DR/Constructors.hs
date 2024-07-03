{-# LANGUAGE DataKinds,
             GADTs, 
             TypeFamilies,
             PolyKinds, 
             TypeOperators,
             MultiParamTypeClasses, 
             FlexibleInstances,
             FlexibleContexts,
             UndecidableInstances #-}

module Constructors where

import AbstractSyntax
import Environment
import Control.Monad.State 
import qualified Data.Map.Strict as M
import Data.Maybe
import TwoLevels

-- Variables
-- var :: HList env -> SNat (n :: Nat) -> Exp env (Lookup env n) '[] (n ': '[])
-- var en n = Var n

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

-- 
lift2 :: (Int -> Int -> Int) -> Exp env st d var1  -> Exp env st' d' var2  -> 
          Exp env (Join st st') (Union d d') (Union var1 var2) 
lift2 f =  OpBin (BinInt f) 

lift :: (Int -> Int) -> Exp env st d var -> Exp env st d var
lift f = OpUn (UnInt f) 
           

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
iff c e1 e2 = If c e1 e2

-- Loop
infixr 6 `while`
while c e1 = While c e1


