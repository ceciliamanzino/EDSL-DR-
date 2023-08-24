{-# LANGUAGE DataKinds,
             GADTs, 
             TypeFamilies,
             PolyKinds, 
             TypeOperators,
             MultiParamTypeClasses, 
             FlexibleInstances,
             FlexibleContexts,
             UndecidableInstances #-}



module DelimitedRelease (Nat(Zero, Succ), SType(Low, High), SeType(L, H), SNat(SZero, SSucc), 
                         Exp(Var, IntLit, BoolLit, Ope, Declassify), 
                         Op(Plus, Minus, Mult, Div, Exp, Mod, And, Or, Gt, GtE, Lt, LtE, Eq, NotEq),
                         Stm(Skip, Ass, Seq, If, While),
                         Lookup, Meet, Union, Intersection) where


-- natural numbers
data Nat = Zero | Succ Nat

-- Security levels at value level
data SType = Low | High


-- conditional at type level (for programmig at type level)
type family IfThenElse (b :: Bool) (t :: a) (u :: a) :: a where
   IfThenElse 'True  t u = t
   IfThenElse 'False t u = u 

--------------------------------------------------
------ Type families on security levels
--------------------------------------------------

--looks up a key in an association list
type family Lookup (env :: [(k,st)]) (n :: k) :: st where 
    Lookup ('(n, st) ': env) n = st 
    Lookup ('(m, st) ': env) n = Lookup env n   


-- maximun of security types
type family Join (st :: SType) (st' :: SType) :: SType where  
   Join 'Low  x = x
   Join 'High x = 'High


-- minimun of security types
type family Meet (st :: SType) (st' :: SType) :: SType where 
   Meet 'Low  x    = 'Low
   Meet 'High x    = x
   Meet x    'High = x         -- needed to simplify typed expressions
   Meet x    'Low  = 'Low


-- order of the security types
class LEq (a :: SType) (b :: SType) 
instance LEq 'Low x 
instance LEq 'High 'High


---------------------------------------------------
---- type families on security environments
---------------------------------------------------

-- list membership predicate (x ∈ xs)  
type family Elem (x :: a) (xs :: [a]) :: Bool where
  Elem x '[]       = 'False
  Elem x (x ': xs) = 'True 
  Elem x (y ': xs) = Elem x xs 


-- append two lists
type family Union (xs :: [a]) (ys :: [a]):: [a] where 
    Union '[]       ys = ys
    Union (x ': xs) ys = x ': (Union xs ys) 


-- (xs ∩ ys) 
type family Intersection (xs :: [a]) (ys :: [a]) :: [a] where
  Intersection '[] ys = '[]
  Intersection xs '[] = '[]
  Intersection (x ': xs) ys = IfThenElse (Elem x ys) 
                                         (x ': Intersection xs ys) 
                                         (Intersection xs ys)

-----------------------------------------------------

-- Singleton type for natural numbers

data SNat (n :: Nat) where
  SZero :: SNat 'Zero
  SSucc :: SNat n -> SNat ('Succ n)


-- Singleton type for security levels

data SeType (s :: SType) where
  L :: SeType 'Low
  H :: SeType 'High


-- aritmethic and boolean operations 
data Op = Plus | Minus| Mult | Div | Exp | Mod 
        | And | Or | Gt | GtE | Lt | LtE | Eq | NotEq 



-----------------------------------
-- Expressions of the language
-----------------------------------

data Exp :: [(Nat,SType)] -> SType -> [Nat] -> [Nat] -> * where
    Var :: SNat (n :: Nat) -> Exp env (Lookup env n) '[] '[n]          -- variables
    IntLit :: Int -> Exp env 'Low '[] '[]                              -- literal numbers
    BoolLit :: Bool -> Exp env 'Low '[] '[]                            -- literal booleans 
    Ope :: Op ->                                                       -- operation
           Exp env st d var1 -> 
           Exp env st' d' var2 -> 
           Exp env (Join st st') (Union d d') (Union var1 var2)
           
    Declassify :: Exp env l' d vars -> SeType l -> Exp env l vars vars -- the expression declassify



------------------------------------
--- Terms of the language
------------------------------------

data Stm :: [(Nat,SType)] -> SType -> [Nat] -> [Nat] -> * where

 Skip :: Stm env 'High '[] '[]                                      -- skip 
 
 Ass :: LEq st (Lookup env n) =>                                    -- assignment
        SNat (n :: Nat) -> 
        Exp env st d var ->  
        Stm env (Lookup env n) '[n] d 

 Seq :: Intersection u1 d2 ~ '[] =>
        Stm env pc u1 d1 -> 
        Stm env pc' u2 d2 ->                                        -- term sequence 
        Stm env (Meet pc pc') (Union u1 u2) (Union d1 d2)

 If  :: LEq st (Meet pc pc') =>                                     -- conditional   
        Exp env st d vars -> 
        Stm env  pc u1 d1 ->
        Stm env  pc' u2 d2 -> 
        Stm env (Meet pc pc') (Union u1 u2) (Union d (Union d1 d2)) 

 While :: (Intersection u1 (Union d d1) ~ '[], LEq st pc) => 
          Exp env st d vars ->                                      -- loop
          Stm env pc u1 d1 ->
          Stm env pc u1 (Union d d1)

     
