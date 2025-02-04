{-# LANGUAGE DataKinds,
             GADTs, 
             TypeFamilies,
             PolyKinds, 
             TypeOperators,
             MultiParamTypeClasses, 
             FlexibleInstances,
             FlexibleContexts,
             UndecidableInstances #-}
             
module DR.ThreeLevels (SType (Low ,High, Medium), Top, Bottom, Meet, Join, SeType (L,H,M), LEq) where
                         
-- Security levels at value level
data SType = Low | High | Medium

-- The greatest and least element of the lattice   
type Top = 'High
type Bottom = 'Low
    
--------------------------------------------------
------ Type families on security types
--------------------------------------------------


-- maximun of security types
type family Join (st :: SType) (st' :: SType) :: SType where  
   Join Bottom  x = x
   Join Top x     = Top
   Join x Bottom  = x
   Join x Top     = Top
   Join x x       = x 

-- minimun of security types
type family Meet (st :: SType) (st' :: SType) :: SType where 
   Meet Bottom  x    = Bottom
   Meet Top x        = x
   Meet x    Top     = x         -- needed to simplify typed expressions
   Meet x    Bottom  = Bottom
   Meet x x          = x 


-- order of the security types
class LEq (a :: SType) (b :: SType) 
instance LEq Bottom x 
instance LEq Top Top
instance LEq 'Medium Bottom

                          
-- Singleton type for security levels

data SeType (s :: SType) where
  L :: SeType Bottom
  H :: SeType Top
  M :: SeType 'Medium
             
             
             
