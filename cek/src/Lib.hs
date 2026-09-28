{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Lib where

-- INTERPRETED LANGUAGE: LAMBDA CALCULUS

data Exp = Ref Var
         | Lam Lambda
         | Exp :@ Exp
         deriving Show

type Env = Var -> D
instance Show Env where show _ = "Env"
defaultEnv :: Env
defaultEnv var = Clo (thunk, \x -> defaultD) where thunk = "" :=> Ref var

data Kont = Mt
          | Ar (Exp, Env, Kont)
          | Fn (Lambda, Env, Kont)
          deriving Show

type Var = String

data D = Clo (Lambda, Env) deriving Show
defaultD = Clo (asChurchNum 0, defaultEnv)

data Lambda = Var :=> Exp
        deriving Show


-- CHURCH ENCODING

ident = "x" :=> Ref "x"

asChurchNum :: Int -> Lambda
asChurchNum n = "f" :=> Lam ("x" :=> (tally n)) where tally :: Int -> Exp
                                                      tally 0 = Ref "x"
                                                      tally n = (Ref "f") :@ (tally $ pred n)

asNat :: Exp -> Int
asNat e = h e 0 where
        h (Ref _) x          = x
        h (Lam (_ :=> t)) x  = h t x
        h (_ :@ t) x         = h t (succ x)


-- THE MACHINE

type S = (Exp, Env, Kont)
stateControl :: S -> Exp
stateControl s = c where (c, _, _) = s

step :: S -> S

-- Evaluating a reference? Look it up in the environment.
-- Evaluating a function application? First evaluate the function.
-- Evaluated the function? Go evaluate the argument term.
-- Evaluated the argument too? Perform the application.

step (Ref x, r, k)
   = (Lam v, r', k) where Clo (v, r') = r(x) 

step (f :@ e, r, k)
   = (f, r, Ar(e, r, k))

step (Lam v, r, Ar(e, r', k))
   = (e, r', Fn(v, r, k))

step (Lam v, r, Fn(x :=> e, r', k))
   = (e, r' // [x ==> Clo(v, r)], k)
   where
           -- "as tuple" syntax.
           (==>) :: a -> b -> (a, b)
           (==>) x y = (x, y)

           -- In x case return the anticipated y, otherwise inquire r'.
           (//) :: Eq a => (a -> b) -> [(a, b)] -> (a -> b)
           (//) r' [(x, y)] = \ input -> if (x == input) then y else r'(input)

unchurch :: S -> Int
unchurch s = asNat $ stateControl s

inject :: Exp -> S
inject (e) = (e, defaultEnv, Mt)

isFinal :: S -> Bool
isFinal (Lam _, _, Mt) = True
isFinal _ = False

eval :: S -> S
eval = until isFinal step 


-- TESTING

s0 = inject (Ref "not_in_env")
-- ghci> stateControl $ eval s0
-- Lam ("" :=> Ref "not_in_env")

r1 :: Env
r1 "eleven" = Clo (asChurchNum 11, defaultEnv)
s1 = (Ref "eleven", r1, Mt)
-- ghci> unchurch $ eval s1
-- 11

s2 = inject (Lam ident :@ (Lam $ asChurchNum 5))
-- ghci> unchurch $ eval s2
-- 5
