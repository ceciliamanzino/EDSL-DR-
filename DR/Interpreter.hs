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
evalExp (IntLit i)                = return i
evalExp (BoolLit i)               = return (fromBool i)
evalExp (Var x)                   = lookfor (toInt x)
evalExp (Declassify e l)          = evalExp e 
evalExp (OpBin (BinInt f) e1 e2)  = evalOp e1 e2 f
evalExp (OpBin (PredInt f) e1 e2) = evalOp e1 e2 (\v v' -> fromBool (f v v'))
evalExp (OpBin (BinBool f) e1 e2) = evalOp e1 e2 (\v v' -> fromBool (f (toBool v) (toBool v')))
evalExp (OpUn (UnInt f) e)        = do v <- evalExp e
                                       return (f v)
evalExp (OpUn (UnBool f) e)       = do v <- evalExp e
                                       return (fromBool (f (toBool v)))



evalOp e1 e2 f = do v1 <- evalExp e1
                    v2 <- evalExp e2
                    return (f v1 v2)
  
fromBool False = 0
fromBool True = 1

toBool 1 = True
toBool 0 = False


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





