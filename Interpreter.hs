{-# LANGUAGE TypeFamilies #-}

module DR.Interpreter where

import DR.AbstractSyntax
import qualified Data.Map.Strict as M
import Control.Monad.Trans.State
import Control.Monad.Trans.Maybe
import Data.Maybe
import Control.Monad.State (lift) 


-- Map between variables and assigned values
type Env = M.Map Int Int


-- Empty environment
initMemory :: Env
initMemory = M.empty
                             
data GlState = GlState {
  en :: Env,         -- environment of values
  trace :: [Int]     -- trace of outputs
}                             
  deriving Show                             
-----------------------------------
---  MonadDR
-----------------------------------

-- Mónada CSP
-- class (MonadState GlState m, MaybeT m) => MonadDR m where

type Result a = StateT GlState (MaybeT IO) a


throw :: Result a 
throw = lift (MaybeT (return Nothing))

updateEn :: Env -> Result ()   
updateEn e = modify (\s-> s {en = e})

update :: Int -> Int -> Result ()   
update v i = do env <- getEnv
                updateEn (M.insert v i env) 

getEnv :: Result Env
getEnv = gets en
          
lookfor :: Int -> Result Int 
lookfor x = do env <- getEnv
               maybe throw return (M.lookup x env)


getOutput :: Result [Int]
getOutput = gets trace

addOutput :: Int -> Result ()
addOutput n = do t <- getOutput
                 modify (\s -> s {trace = t ++[n]}) 

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
eval :: Stm a b c d -> Env -> IO GlState
eval p env = do m <- runMaybeT $ do (v,s) <- runStateT (evalStm p False) (GlState env [])
                                    return s   
                                    
                return (fromJust m)                      

-- Evaluates a term given an empty state.
eval' :: Stm a b c d -> IO GlState
eval' p =  eval p initMemory




