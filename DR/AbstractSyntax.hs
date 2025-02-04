{-# LANGUAGE DataKinds,
             GADTs, 
             TypeFamilies,
             PolyKinds, 
             TypeOperators,
             MultiParamTypeClasses, 
             FlexibleInstances,
             FlexibleContexts,
             UndecidableInstances #-}



module DR.AbstractSyntax (Nat(Zero, Succ), SNat(SZero, SSucc), Lookup, Elem, Union, Intersection,  
                          Exp(Var, IntLit, BoolLit, Declassify, OpBin, OpUn),
                          Op (BinInt, BinBool, PredInt, UnInt, UnBool),  
                          Stm(Skip, Ass, Seq, If, While, Output), toInt) where

-- Lattice of three security levels
import DR.ThreeLevels

-- natural numbers
data Nat = Zero | Succ Nat

-----------------------------------------------------

-- Singleton type for natural numbers
-----------------------------------------------------

data SNat (n :: Nat) where
  SZero :: SNat 'Zero
  SSucc :: SNat n -> SNat ('Succ n)

-- convert a value of type SNat n to a natural of type Int that it represents    
toInt :: SNat n -> Int
toInt SZero = 0
toInt (SSucc x) = 1 + (toInt x)

-- conditional at type level (for programmig at type level)
type family IfThenElse (b :: Bool) (t :: a) (u :: a) :: a where
   IfThenElse 'True  t u = t
   IfThenElse 'False t u = u 

--------------------------------------------------
------ Type families on security environments
--------------------------------------------------

--looks up a key in an association list
type family Lookup (env :: [(k,st)]) (n :: k) :: st where 
    Lookup ('(n, st) ': env) n = st 
    Lookup ('(m, st) ': env) n = Lookup env n   


-- list membership predicate (x ∈ xs)  
type family Elem (x :: a) (xs :: [a]) :: Bool where
  Elem x '[]       = 'False
  Elem x (x ': xs) = 'True 
  Elem x (y ': xs) = Elem x xs 

-- append two lists
type family Union (xs :: [a]) (ys :: [a]):: [a] where
    Union xs xs = xs
    Union xs '[] = xs 
    Union '[]       ys = ys
    Union (x ': xs) ys = x ': (Union xs ys)
     

-- (xs ∩ ys) 
type family Intersection (xs :: [a]) (ys :: [a]) :: [a] where
  Intersection xs xs = xs
  Intersection '[] ys = '[]
  Intersection xs '[] = '[]
  Intersection (x ': xs) ys = IfThenElse (Elem x ys) 
                                         (x ': Intersection xs ys) 
                                         (Intersection xs ys)
   
------------------------------------------
type family First (xs :: [(Nat,st)]) (e :: Nat) :: Nat where 
    First '[] e = e 
    First ('(m, x) ': env) e = Succ m   

--------------------------------------
-------- The Language  ---------------
--------------------------------------

-- aritmethic and boolean operations 
data Op = BinInt (Int -> Int -> Int) | BinBool (Bool -> Bool -> Bool) | 
          PredInt (Int -> Int -> Bool) | UnInt (Int -> Int) | UnBool (Bool -> Bool)  

-----------------------------------
-- Expressions of the language
-----------------------------------

data Exp :: [(Nat,SType)] -> SType -> [Nat] -> [Nat] -> * where
    Var :: SNat (n :: Nat) -> Exp env (Lookup env n) '[] '[n]         -- variables
    IntLit :: Int -> Exp env Bottom '[] '[]                           -- literal numbers
    BoolLit :: Bool -> Exp env Bottom '[] '[]                         -- literal booleans 
    OpBin :: Op ->                                                    -- binary operation
           Exp env st d var1  -> 
           Exp env st' d' var2  -> 
           Exp env (Join st st') (Union d d') (Union var1 var2) 
    OpUn  :: Op ->                                                    -- unary operation
           Exp env st d var  -> 
           Exp env st d var         
    Declassify :: Exp env l' d vars -> SeType l -> Exp env l vars vars  -- the expression declassify
 

------------------------------------
--- Terms of the language
------------------------------------

data Stm :: [(Nat,SType)] -> SType -> [Nat] -> [Nat] -> * where

 Skip :: Stm env Top '[] '[]                                      -- skip 
 
 Ass :: LEq st (Lookup env n) =>                                    -- assignment
        SNat (n :: Nat) -> 
        Exp env st d var ->  
        Stm env (Lookup env n) '[n] d 

 Seq :: Intersection u1 d2 ~ '[] =>
        Stm env pc u1 d1 -> 
        Stm env pc' u2 d2 ->                                        -- term sequence 
        Stm env (Meet pc pc') (Union u1 u2) (Union d1 d2)

 If  :: LEq st (Meet pc pc') =>                                     -- conditional   
        Exp env st d vars  -> 
        Stm env  pc u1 d1 ->
        Stm env  pc' u2 d2 -> 
        Stm env (Meet pc pc') (Union u1 u2) (Union d (Union d1 d2)) 

 While :: (Intersection u1 (Union d d1) ~ '[] , LEq st pc) => 
          Exp env st d vars ->                                      -- loop
          Stm env pc u1 d1 ->
          Stm env pc u1 (Union d d1)

 Output :: Exp env Bottom d var ->                                  -- public output  
           Stm env Bottom '[] d     
           
            
           
           
