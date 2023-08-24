{-# LANGUAGE TypeFamilies #-}

module Interpreter where

import AbstractSyntax
import qualified Data.Map.Strict as M
import Data.Maybe
import Control.Monad.State 

-- Map between variables and assigned values
type Env = M.Map Int Int


-- Empty environment
initMemory :: Env
initMemory = M.empty


type Result a = StateT Env Maybe a
  

-- convert a value of type SNat n to a natural of type Int that it represents    
toInt :: SNat n -> Int
toInt SZero = 0
toInt (SSucc x) = 1 + (toInt x)

update :: Int -> Int -> Result ()   
update v i = StateT (\s -> Just ((), M.insert v i s))  

lookfor :: Int -> Result Int 
lookfor x = do env <- get
               maybe throw return (M.lookup x env)

throw :: Result a
throw = StateT (\s -> Nothing)


-- Evaluates a term given a state.
eval :: Stm a b c d -> Env -> Env
eval p env = snd $ fromJust $ (runStateT (evalStm p) env)


-- Evaluates a term given an empty state.
evalIni :: Stm a b c d -> Env
evalIni p =  eval p initMemory


--------------------------------------- 
-- Expression evaluator
---------------------------------------

evalExp ::  Exp a b c d -> Result Int
evalExp (IntLit i)        = return i
evalExp (BoolLit i)       = return (boolToInt i)
evalExp (Var x)           = lookfor (toInt x)
evalExp (Declassify e l)  = evalExp e 
evalExp (Ope Plus e1 e2)  = evalIntOp e1 e2 (+)
evalExp (Ope Minus e1 e2) = evalIntOp e1 e2 (-)
evalExp (Ope Mult e1 e2)  = evalIntOp e1 e2 (*)
evalExp (Ope Div e1 e2)   = do v1 <- evalExp e1
                               v2 <- evalExp e2
                               return (div v1 v2)
evalExp (Ope Exp e1 e2)   = evalIntOp e1 e2 (^)
evalExp (Ope Mod e1 e2)   = evalIntOp e1 e2 (mod)
evalExp (Ope And e1 e2)   = evalIntOp e1 e2 (andOp) 
evalExp (Ope Or e1 e2)    = evalIntOp e1 e2 (orOp) 
evalExp (Ope Gt e1 e2)    = evalBoolOp e1 e2 (>)
evalExp (Ope GtE e1 e2)   = evalBoolOp e1 e2 (>=) 
evalExp (Ope Lt e1 e2)    = evalBoolOp e1 e2 (<) 
evalExp (Ope LtE e1 e2)   = evalBoolOp e1 e2 (<=) 
evalExp (Ope Eq e1 e2)    = evalBoolOp e1 e2 (==) 
evalExp (Ope NotEq e1 e2) = evalBoolOp e1 e2 (/=) 


evalBoolOp e1 e2 f = do v1 <- evalExp e1
                        v2 <- evalExp e2
                        return (boolToInt (f v1 v2))

evalIntOp e1 e2 f = do v1 <- evalExp e1
                       v2 <- evalExp e2
                       return (f v1 v2)
  
boolToInt False = 0
boolToInt True = 1

andOp 0 x = 0
andOp x 0 = 0
andOp x y = 1

orOp 0 0 = 0
orOp x y = 1

---------------------------------------
-- Term evaluator
---------------------------------------
      

-- Evaluates multiple steps of a command, until it reaches Skip
evalStm :: Stm a b c d -> Result ()
evalStm Skip          = return ()
evalStm (Ass x e)     = do v <- evalExp e
                           update (toInt x) v

evalStm (Seq c0  c1)  =  evalStm c0 >> (evalStm c1) 
evalStm (If b c0 c1)  = do vb <- evalExp b
                           if vb == 1 then evalStm c0 else evalStm c1 
evalStm w@(While b c) = do vb <- evalExp b
                           if vb == 1 then evalStm (Seq c w) else return () 





