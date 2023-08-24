{-# LANGUAGE DataKinds,
             TypeOperators,
             KindSignatures,
             GADTs #-}

module Environment where

import AbstractSyntax


{-

HList is a data type defined to represent the environment as list of types. 
All of his elements have kind (Nat, SType).
See below an example on how to use this data type.

-}

infixr 5 :-:

data HList :: [(Nat , SType)] -> * where
    Nil :: HList '[] 
    (:-:) :: (SNat n , SeType s) -> HList xs -> HList ('(n , s) ': xs) 


-- types of the variables 

type Zero = 'Zero
type One = 'Succ 'Zero
type Two = 'Succ One
type Three = 'Succ Two
type Four = 'Succ Three
type Five = 'Succ Four
type Six = 'Succ Five
type Seven = 'Succ Six
type Eight = 'Succ Seven
type Nine = 'Succ Eight

-- Variables

zero  :: SNat Zero
zero = SZero

one  :: SNat One
one = SSucc SZero

two  :: SNat Two
two = SSucc one

three :: SNat Three
three = SSucc two

four :: SNat Four
four = SSucc three

five :: SNat Five
five = SSucc four

six :: SNat Six
six = SSucc five

seven :: SNat Seven
seven = SSucc six

eight :: SNat Eight
eight = SSucc seven

nine :: SNat Nine
nine = SSucc eight


{-

We can build our own environment using HList data type, using the numbers defined previously.
This way we can build a value-level list, that contains a heterogeneous list of types as a type. 
For example: 

env = (zero, L) :-: (one, H) :-: (two, H) :-: (three, H) :-: (four, H) :-: Nil

-}








