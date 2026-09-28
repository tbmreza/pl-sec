module Lib where

import Text.Printf (printf)

example :: Term
example = Var 0 0 0

---------------------------------------------------------
-- AST: Lambda calculus variables as de Bruijn indices --
---------------------------------------------------------

data Term = Var Info   Int TotalLength
          | Abs Info   Name Term
          | App Info   Term Term
    deriving Show

type Info = Int  -- term's position at source
type TotalLength = Int  -- length of naming context in which Var occurs
type Name = String  -- annotation for printing de Bruijn indices

data Binding = NameBind
          -- | not really used in this untyped system

type Context = [(String, Binding)]

showtm :: Context -> Term -> String
showtm ctx t =
    case t of
        Abs _info name body ->
            printf "(lambda %s. %s)" name' (showtm ctx' body)
                where (ctx', name') = pickfreshname ctx name

        App _info t1 t2 ->
            printf "(%s %s)" (showtm ctx t1) (showtm ctx t2)

        Var info i expectedTotalLength -> if length ctx /= expectedTotalLength
            then "[bad index]"
            else index2name info ctx i


index2name :: Info -> Context -> Int -> String  -- partial!
index2name info ctx i = name
    where (name, _) = ctx !! i


pickfreshname :: Context -> String -> (Context, String)
pickfreshname ctx name =
    if isnamebound ctx name
        then pickfreshname ctx (name ++ "'")
        else ((name, NameBind):ctx, name) where

    isnamebound :: Context -> String -> Bool
    isnamebound ctx name =
        case ctx of
            [] -> False
            (inList, _binding):rest -> if name == inList
                then True
                else isnamebound rest name


------------------------------
-- Reduction and evaluation --
------------------------------

termShift :: Int -> Term -> Term  -- d for distance, delta, maybe something else
termShift d t =
    walk 0 t where
    walk cutoff t = case t of
        Var info i expectedTotalLength -> if i >= cutoff
            then Var info (i+d) (expectedTotalLength+d)
            else Var info i (expectedTotalLength+d)

        Abs info name body ->
            Abs info name (walk (cutoff+1) body)

        App info t1 t2 ->
            App info (walk cutoff t1) (walk cutoff t2)


-- Substitute term s for variable-number j.
termSubst :: Int -> Term -> Term -> Term
termSubst j s t =
    walk 0 t where
    walk cutoff t = case t of
        Var info i expectedTotalLength -> if i == (j+cutoff)
            then termShift cutoff s
            else Var info i expectedTotalLength

        Abs info name body ->
            Abs info name (walk (cutoff+1) body)

        App info t1 t2 ->
            App info (walk cutoff t1) (walk cutoff t2)


termSubstTop :: Term -> Term -> Term
termSubstTop s t = termShift (-1) (termSubst 0 (termShift 1 s) t)


isval :: Context -> Term -> Bool
isval _ t = case t of
    Abs _ _ _ -> True
    _ -> False


eval1 :: Context -> Term -> Maybe Term

eval1 ctx
    t@(App _ (Abs _info _name t12) v2)
    | isval ctx v2 =

    Just (termSubstTop v2 t12)

eval1 ctx
    t@(App info v1 t2)
    | isval ctx v1 =

    Just (App info v1 t2')
        where (Just t2') = eval1 ctx t2

eval1 ctx
    t@(App info t1 t2) =

    Just (App info t1' t2)
        where (Just t1') = eval1 ctx t1

eval1 ctx _ = Nothing


eval :: Context -> Term -> Term
eval ctx t =
    case eval1 ctx t of
        Nothing -> t
        Just t' -> eval ctx t'
