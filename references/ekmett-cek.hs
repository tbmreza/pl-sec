--- https://gist.github.com/ekmett/f081b5e36bac3fed1ea6b21eb25327c6
--- Simple CEK Machine

{- repl test recap:

input                                                  State Exp Env Kont
step (start id_)                                       State  (Lam "x" (Var "x"))  Env Top
step (start (Ap id_ id_))                              State  (Lam "x" (Var "x"))  Env (Arg (Lam "x" (Var "x")) Env Top)
step (step (start (Ap id_ id_)))                       State  (Lam "x" (Var "x"))  Env (Fun "x" (Var "x") Env Top)
step (step (step (start (Ap id_ id_))))                State  (Var "x")            Env Top
step (step (step (step (start (Ap id_ id_)))))         State  (Lam "x" (Var "x"))  Env Top
step (step (step (step (step (start (Ap id_ id_))))))  State  (Lam "x" (Var "x"))  Env Top -}

{-# language StrictData #-}
module CEK where

-- C   -- Control
-- E   -- Environment
-- (S) -- Store
-- K   -- Continuation

data Exp
  = Var String
  | Lam String Exp
  | Ap Exp Exp 
  deriving Show

newtype Env = Env { (!) :: String -> Value }

instance Show Env where
  show _ = "Env"

data Value = Closure String Exp Env

data Kont
  = Top
  | Arg Exp Env Kont
  | Fun String Exp Env Kont
  deriving Show

data State = State Exp Env Kont
  deriving Show

start :: Exp -> State
start c = State c (Env $ const undefined) Top

id_ = Lam "x" $ Var "x"
const_ = Lam "x" $ Lam "y" $ Var "x"

-- small-step semantics step
step :: State -> State
step s@(State c e k) = case c of
  Var v -> case e ! v of
    Closure v' b e' -> State (Lam v' b) e' k
  Ap cf cx -> State cf e (Arg cx e k)
  Lam v b -> case k of
    Top -> s
    Arg cx e' k' -> State cx e' (Fun v b e k')
    Fun v' b' e' k' -> State b' (extend v' (Closure v b e) e') k'

extend :: String -> Value -> Env -> Env
extend i v f = Env $ \j -> if i == j then v else f ! j

final :: State -> Bool
final (State Lam{} _ Top) = True
final _ = False

-- until :: (a -> Bool) -> (a -> a) -> a -> a

eval :: State -> State
eval = until final step 
