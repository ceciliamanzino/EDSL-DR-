{-# LANGUAGE TypeFamilies #-}

module DR.Interpreter where

import DR.AbstractSyntax
import qualified Data.Map.Strict as M
import Data.Maybe
import Control.Monad.State 
import Control.Monad.Trans.Maybe

-- Map between variables and assigned values
type Env = M.Map Int Int


-- Empty environment
initMemory :: Env
initMemory = M.empty
                             
type Result a = StateT (Env,[Int]) (MaybeT IO) a

-----------------------------------
---  Monad operations
-----------------------------------

throw :: Result a  
throw = lift (MaybeT (return Nothing))

update :: Int -> Int -> Result ()   
update v i = StateT (\(env, out) -> MaybeT ( return (Just ((), (M.insert v i env, out)))))  

lookfor :: Int -> Result Int 
lookfor x = do (env, out) <- get
               maybe throw return (M.lookup x env)

addOutput :: Int -> Result ()
addOutput n = StateT (\(env, out) -> MaybeT ( return (Just ((), (env, out++[n])))))

getOutput :: Result [Int]
getOutput = do (env, out) <- get
               return out 

fromIO :: IO a -> Result a
fromIO = lift . lift

outPut :: String -> Result ()
outPut s = fromIO (putStrLn s)


outPuts :: [Int] -> Result ()
outPuts (x:xs) = outPut (show x) >> outPuts xs
outPuts []     = return ()  

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
-- Evaluate an statements
evalStm :: Stm a b c d -> Bool -> Result ()
evalStm Skip       _   = return ()
evalStm (Ass x e)  _   = do v <- evalExp e
                            update (toInt x) v
                            
evalStm (Seq c0  c1) b =  evalStm c0 b >> (evalStm c1 b)  

evalStm (If e c0 c1) b = do n <- evalExp e
                            if n == 1 then evalStm c0 b else evalStm c1 b 

evalStm w@(While e c) _ = do vb <- evalExp e
                             if vb == 1 then evalStm (Seq c w) True 
                             else do out <- getOutput  -- When the loop terminates show the output
                                     outPuts out  

-- if the argument b is true put the output in the state 
evalStm (Output e) b = do n <- evalExp e
                          if b then addOutput n else outPut (show n)


-- Evaluates a term given a state.
eval :: Stm a b c d -> Env -> IO Env
eval p env = do m <- runMaybeT $ do (v, (env', o)) <- runStateT (evalStm p False) (env, []) 
                                    return env'
                return (fromJust m)                      

-- Evaluates a term given an empty state.
eval' :: Stm a b c d -> IO Env
eval' p =  eval p initMemory




