module Lib
    ( someFunc
    ) where

import Text.Printf (printf)
import Control.Exception (Exception, throwIO)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-------------------------------------------------
-- AST: `lambdacalc` with boolean constructors --
-------------------------------------------------

data Term = Var Info   Int TotalLength
          | Abs Info   Name Ty Term
          -- | Abs Info   Name Term
          | App Info   Term Term

          | TmIf Info  Term Term Term
          | TmTrue Info
          | TmFalse Info
    deriving Show

type Info = Int  -- term's position at source
type TotalLength = Int  -- length of naming context in which Var occurs
type Name = String  -- annotation for printing de Bruijn indices

data Binding = NameBind
             | VarBind Ty

type Context = [(String, Binding)]

data Ty = TyBool
        | TyArr Ty Ty
    deriving (Show, Eq)


addbinding :: Context -> String -> Binding -> Context
addbinding ctx var bind = (var, bind):ctx

typeFromContext :: Info -> Context -> Int -> Ty
typeFromContext info ctx i = ty
    where VarBind ty = bindingFrom info ctx i

bindingFrom :: Info -> Context -> Int -> Binding  -- partial!
bindingFrom info ctx i = binding
    where (_, binding) = ctx !! i


showtm :: Context -> Term -> String
showtm ctx t =
    case t of
        Abs _info name boundTy body ->
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

        Abs info name boundTy body ->
            Abs info name boundTy (walk (cutoff+1) body)

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

        Abs info name boundTy body ->
            Abs info name boundTy (walk (cutoff+1) body)

        App info t1 t2 ->
            App info (walk cutoff t1) (walk cutoff t2)


termSubstTop :: Term -> Term -> Term
termSubstTop s t = termShift (-1) (termSubst 0 (termShift 1 s) t)


isval :: Context -> Term -> Bool
isval _ t = case t of
    Abs _ _ _ _ -> True
    _ -> False


eval1 :: Context -> Term -> Maybe Term

eval1 ctx
    t@(App _ (Abs _info _name _ t12) v2)
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

instance Exception General
data General = DifferentArmsTypes
             | NonBooleanGuard
             | ParameterTypeMismatch
             | ArrowTypeExpected
             -- ??: exception ctor that's parametrized
             | WrongKindOfBinding
             | VariableLookup
    deriving (Show)


typeof :: Context -> Term -> IO Ty  -- IO throws on ill-typed terms
typeof ctx t = case t of
    TmTrue _info  -> return TyBool
    TmFalse _info -> return TyBool

    TmIf info t1 t2 t3 -> do
        pred <- typeof ctx t1
        case pred of
            TyBool -> do
                tyT2 <- typeof ctx t2
                tyT3 <- typeof ctx t3
                if tyT2 /= tyT3 then throwIO DifferentArmsTypes
                else return tyT2

            _ -> throwIO NonBooleanGuard


    Var info i _len -> return $ typeFromContext info ctx i

    Abs _info name boundTy body -> do
        let ctx' = addbinding ctx name (VarBind boundTy)
        tyT2 <- typeof ctx' body
        return $ TyArr tyT2 tyT2

    App _info t1 t2 -> do
        tyT1 <- typeof ctx t1
        tyT2 <- typeof ctx t2
        case tyT1 of
            TyArr tyT11 tyT12 -> if tyT2 /= tyT11
                then throwIO ParameterTypeMismatch
                else return tyT12
        throwIO ArrowTypeExpected
