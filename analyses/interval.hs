{-
  Elegant Abstract Interpretation Framework in Haskell

  This implementation provides an elegant abstract interpretation framework
  for analyzing numerical properties of a small imperative language.
  
  The framework includes:
  1. An abstract domain for interval analysis
  2. A small imperative language AST
  3. An abstract interpreter that computes program invariants
-}

module AbstractInterpretation where

import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

-- ===== Abstract Domain: Intervals =====

data Interval = Interval
  { lower :: Double
  , upper :: Double
  } | Bottom
  deriving (Eq)

instance Show Interval where
  show Bottom = "⊥"
  show (Interval l u)
    | l == (-infinity) && u == infinity = "⊤"
    | otherwise = "[" ++ show l ++ ", " ++ show u ++ "]"

infinity :: Double
infinity = 1/0

top :: Interval
top = Interval (-infinity) infinity

bottom :: Interval
bottom = Bottom

join :: Interval -> Interval -> Interval
join Bottom i = i
join i Bottom = i
join (Interval l1 u1) (Interval l2 u2) = Interval (min l1 l2) (max u1 u2)

meet :: Interval -> Interval -> Interval
meet Bottom _ = Bottom
meet _ Bottom = Bottom
meet (Interval l1 u1) (Interval l2 u2)
  | max l1 l2 <= min u1 u2 = Interval (max l1 l2) (min u1 u2)
  | otherwise = Bottom

widen :: Interval -> Interval -> Interval
widen Bottom i = i
widen i Bottom = i
widen (Interval l1 u1) (Interval l2 u2) = Interval l' u'
  where
    l' = if l2 < l1 then (-infinity) else l1
    u' = if u2 > u1 then infinity else u1

narrow :: Interval -> Interval -> Interval
narrow Bottom _ = Bottom
narrow _ Bottom = Bottom
narrow (Interval l1 u1) (Interval l2 u2) = Interval l' u'
  where
    l' = if l1 == (-infinity) then l2 else l1
    u' = if u1 == infinity then u2 else u1

addInterval :: Interval -> Interval -> Interval
addInterval Bottom _ = Bottom
addInterval _ Bottom = Bottom
addInterval (Interval l1 u1) (Interval l2 u2) =
  Interval (l1 + l2) (u1 + u2)

subInterval :: Interval -> Interval -> Interval
subInterval Bottom _ = Bottom
subInterval _ Bottom = Bottom
subInterval (Interval l1 u1) (Interval l2 u2) =
  Interval (l1 - u2) (u1 - l2)

mulInterval :: Interval -> Interval -> Interval
mulInterval Bottom _ = Bottom
mulInterval _ Bottom = Bottom
mulInterval (Interval l1 u1) (Interval l2 u2) =
  let products = [l1 * l2, l1 * u2, u1 * l2, u1 * u2]
  in Interval (minimum products) (maximum products)

divInterval :: Interval -> Interval -> Interval
divInterval Bottom _ = Bottom
divInterval _ Bottom = Bottom
divInterval (Interval l1 u1) (Interval l2 u2)
  | l2 == 0 && u2 == 0 = Bottom
  | l2 <= 0 && u2 >= 0 = top -- A possible zero divisor makes the result unbounded.
  | l2 > 0 = Interval (l1 / u2) (u1 / l2)
  | u2 < 0 = Interval (u1 / u2) (l1 / l2)
  | otherwise = top

-- ===== Abstract Environment =====

type Env = Map.Map String Interval

emptyEnv :: Env
emptyEnv = Map.empty

lookupVar :: String -> Env -> Interval
lookupVar var env = fromMaybe Bottom (Map.lookup var env)

updateVar :: String -> Interval -> Env -> Env
updateVar = Map.insert

joinEnv :: Env -> Env -> Env
joinEnv env1 env2 =
  let allVars = Map.keys env1 ++ Map.keys env2
      uniqueVars = foldr (\x acc -> if x `elem` acc then acc else x:acc) [] allVars
  in foldr (\var acc ->
      let val1 = lookupVar var env1
          val2 = lookupVar var env2
      in updateVar var (join val1 val2) acc)
    emptyEnv uniqueVars

-- ===== Abstract Syntax Tree =====

data Expr
  = Lit Double
  | Var String
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Neg Expr
  deriving Show

data BoolExpr
  = Lt Expr Expr
  | Lte Expr Expr
  | Gt Expr Expr
  | Gte Expr Expr
  | Eq Expr Expr
  | Neq Expr Expr
  | And BoolExpr BoolExpr
  | Or BoolExpr BoolExpr
  | Not BoolExpr
  deriving Show

data Stmt
  = Assign String Expr
  | If BoolExpr Stmt Stmt
  | While BoolExpr Stmt
  | Seq [Stmt]
  | Skip
  deriving Show

evalExpr :: Expr -> Env -> Interval
evalExpr (Lit n) _ = Interval n n
evalExpr (Var x) env = lookupVar x env
evalExpr (Add e1 e2) env = addInterval (evalExpr e1 env) (evalExpr e2 env)
evalExpr (Sub e1 e2) env = subInterval (evalExpr e1 env) (evalExpr e2 env)
evalExpr (Mul e1 e2) env = mulInterval (evalExpr e1 env) (evalExpr e2 env)
evalExpr (Div e1 e2) env = divInterval (evalExpr e1 env) (evalExpr e2 env)
evalExpr (Neg e) env =
  case evalExpr e env of
    Bottom -> Bottom
    Interval l u -> Interval (-u) (-l)

data AbstractBool = ABTrue | ABFalse | ABMaybe
  deriving (Show, Eq)

evalBoolExpr :: BoolExpr -> Env -> AbstractBool
evalBoolExpr (Lt e1 e2) env =
  case (evalExpr e1 env, evalExpr e2 env) of
    (Bottom, _) -> ABFalse
    (_, Bottom) -> ABFalse
    (Interval l1 u1, Interval l2 u2)
      | u1 < l2 -> ABTrue
      | l1 >= u2 -> ABFalse
      | otherwise -> ABMaybe

evalBoolExpr (Lte e1 e2) env =
  case (evalExpr e1 env, evalExpr e2 env) of
    (Bottom, _) -> ABFalse
    (_, Bottom) -> ABFalse
    (Interval l1 u1, Interval l2 u2)
      | u1 <= l2 -> ABTrue
      | l1 > u2 -> ABFalse
      | otherwise -> ABMaybe

evalBoolExpr (Gt e1 e2) env = evalBoolExpr (Lt e2 e1) env
evalBoolExpr (Gte e1 e2) env = evalBoolExpr (Lte e2 e1) env

evalBoolExpr (Eq e1 e2) env =
  case (evalExpr e1 env, evalExpr e2 env) of
    (Bottom, _) -> ABFalse
    (_, Bottom) -> ABFalse
    (Interval l1 u1, Interval l2 u2)
      | l1 == u1 && l2 == u2 && l1 == l2 -> ABTrue
      | l1 > u2 || l2 > u1 -> ABFalse
      | otherwise -> ABMaybe

evalBoolExpr (Neq e1 e2) env =
  case evalBoolExpr (Eq e1 e2) env of
    ABTrue -> ABFalse
    ABFalse -> ABTrue
    ABMaybe -> ABMaybe

evalBoolExpr (And b1 b2) env =
  case (evalBoolExpr b1 env, evalBoolExpr b2 env) of
    (ABFalse, _) -> ABFalse
    (_, ABFalse) -> ABFalse
    (ABTrue, ABTrue) -> ABTrue
    _ -> ABMaybe

evalBoolExpr (Or b1 b2) env =
  case (evalBoolExpr b1 env, evalBoolExpr b2 env) of
    (ABTrue, _) -> ABTrue
    (_, ABTrue) -> ABTrue
    (ABFalse, ABFalse) -> ABFalse
    _ -> ABMaybe

evalBoolExpr (Not b) env =
  case evalBoolExpr b env of
    ABTrue -> ABFalse
    ABFalse -> ABTrue
    ABMaybe -> ABMaybe

filterEnv :: BoolExpr -> Env -> Env -> (Env, Env)
filterEnv (Lt (Var x) (Lit n)) env _ =
  case lookupVar x env of
    Bottom -> (env, env)
    Interval l u ->
      -- Closed intervals cannot represent strict bounds exactly; retaining n
      -- is a sound over-approximation for both branches.
      let trueEnv = updateVar x (meet (Interval l u) (Interval (-infinity) n)) env
          falseEnv = updateVar x (meet (Interval l u) (Interval n infinity)) env
      in (trueEnv, falseEnv)
filterEnv (Gt (Var x) (Lit n)) env _ =
  case lookupVar x env of
    Bottom -> (env, env)
    Interval l u ->
      let trueEnv = updateVar x (meet (Interval l u) (Interval n infinity)) env
          falseEnv = updateVar x (meet (Interval l u) (Interval (-infinity) n)) env
      in (trueEnv, falseEnv)
filterEnv b env _ =
  case evalBoolExpr b env of
    ABTrue -> (env, bottom)
    ABFalse -> (bottom, env)
    ABMaybe -> (env, env)

interpretStmt :: Stmt -> Env -> Int -> Env
interpretStmt Skip env _ = env
interpretStmt (Assign x e) env _ = updateVar x (evalExpr e env) env
interpretStmt (Seq []) env _ = env
interpretStmt (Seq (s:ss)) env k = interpretStmt (Seq ss) (interpretStmt s env k) k

interpretStmt (If cond thenBranch elseBranch) env k =
  let (trueEnv, falseEnv) = filterEnv cond env env
      thenResult = interpretStmt thenBranch trueEnv k
      elseResult = interpretStmt elseBranch falseEnv k
  in joinEnv thenResult elseResult

interpretStmt w@(While cond body) env k
  | k <= 0 = top
  | otherwise =
      let (trueEnv, falseEnv) = filterEnv cond env env
          bodyResult = interpretStmt body trueEnv (k-1)
          fixpoint = fixpointIteration w env bodyResult (k-1)
          (_, finalFalseEnv) = filterEnv cond fixpoint fixpoint
      in finalFalseEnv

fixpointIteration :: Stmt -> Env -> Env -> Int -> Env
fixpointIteration (While cond body) prevEnv currEnv k
  | k <= 0 = joinEnv prevEnv currEnv
  | otherwise =
      let widenedEnv = widenEnv prevEnv currEnv
          (trueEnv, _) = filterEnv cond widenedEnv widenedEnv
          newEnv = if allBottom trueEnv
                   then widenedEnv
                   else interpretStmt body trueEnv k
          isFixpoint = envSubset newEnv widenedEnv
      in if isFixpoint
         then narrowEnv widenedEnv newEnv
         else fixpointIteration (While cond body) widenedEnv (joinEnv widenedEnv newEnv) (k-1)

allBottom :: Env -> Bool
allBottom env = all (== Bottom) (Map.elems env)

envSubset :: Env -> Env -> Bool
envSubset env1 env2 =
  all (\(var, val1) ->
    let val2 = lookupVar var env2
    in intervalSubset val1 val2) (Map.toList env1)

intervalSubset :: Interval -> Interval -> Bool
intervalSubset Bottom _ = True
intervalSubset _ Bottom = False
intervalSubset (Interval l1 u1) (Interval l2 u2) = l2 <= l1 && u1 <= u2

widenEnv :: Env -> Env -> Env
widenEnv env1 env2 =
  let allVars = Map.keys env1 ++ Map.keys env2
      uniqueVars = foldr (\x acc -> if x `elem` acc then acc else x:acc) [] allVars
  in foldr (\var acc ->
      let val1 = lookupVar var env1
          val2 = lookupVar var env2
      in updateVar var (widen val1 val2) acc)
    emptyEnv uniqueVars

narrowEnv :: Env -> Env -> Env
narrowEnv env1 env2 =
  let allVars = Map.keys env1 ++ Map.keys env2
      uniqueVars = foldr (\x acc -> if x `elem` acc then acc else x:acc) [] allVars
  in foldr (\var acc ->
      let val1 = lookupVar var env1
          val2 = lookupVar var env2
      in updateVar var (narrow val1 val2) acc)
    emptyEnv uniqueVars

abstractInterpret :: Stmt -> Env -> Env
abstractInterpret stmt initialEnv = interpretStmt stmt initialEnv 100

fibonacciProgram :: Stmt
fibonacciProgram = Seq
  [ Assign "n" (Lit 10)
  , Assign "a" (Lit 0)
  , Assign "b" (Lit 1)
  , Assign "i" (Lit 0)
  , While (Lt (Var "i") (Var "n"))
      (Seq
        [ Assign "temp" (Var "a")
        , Assign "a" (Var "b")
        , Assign "b" (Add (Var "temp") (Var "b"))
        , Assign "i" (Add (Var "i") (Lit 1))
        ]
      )
  ]

runExample :: IO ()
runExample = do
  let initialEnv = emptyEnv
      resultEnv = abstractInterpret fibonacciProgram initialEnv
  putStrLn "Abstract Interpretation of Fibonacci program:"
  putStrLn "--------------------------------------------"
  mapM_ (\(var, val) -> putStrLn $ var ++ " = " ++ show val) (Map.toList resultEnv)
  putStrLn "--------------------------------------------"
  putStrLn "The result of Fibonacci(10) is in variable 'a'"

main :: IO ()
main = runExample
