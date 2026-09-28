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

-- | The abstract domain of intervals
data Interval = Interval 
  { lower :: Double   -- Can be -infinity
  , upper :: Double   -- Can be infinity
  } | Bottom
  deriving (Eq)

instance Show Interval where
  show Bottom = "⊥"
  show (Interval l u)
    | l == (-infinity) && u == infinity = "⊤"
    | otherwise = "[" ++ show l ++ ", " ++ show u ++ "]"

-- | Positive and negative infinity
infinity :: Double
infinity = 1/0

-- | The top element of the lattice
top :: Interval
top = Interval (-infinity) infinity

-- | The bottom element of the lattice
bottom :: Interval
bottom = Bottom

-- | Least upper bound (join) operation
join :: Interval -> Interval -> Interval
join Bottom i = i
join i Bottom = i
join (Interval l1 u1) (Interval l2 u2) = Interval (min l1 l2) (max u1 u2)

-- | Greatest lower bound (meet) operation
meet :: Interval -> Interval -> Interval
meet Bottom _ = Bottom
meet _ Bottom = Bottom
meet (Interval l1 u1) (Interval l2 u2)
  | max l1 l2 <= min u1 u2 = Interval (max l1 l2) (min u1 u2)
  | otherwise = Bottom

-- | Widening operation to ensure termination
widen :: Interval -> Interval -> Interval
widen Bottom i = i
widen i Bottom = i
widen (Interval l1 u1) (Interval l2 u2) = Interval l' u'
  where
    l' = if l2 < l1 then (-infinity) else l1
    u' = if u2 > u1 then infinity else u1

-- | Narrowing operation to improve precision after widening
narrow :: Interval -> Interval -> Interval
narrow Bottom _ = Bottom
narrow _ Bottom = Bottom
narrow (Interval l1 u1) (Interval l2 u2) = Interval l' u'
  where
    l' = if l1 == (-infinity) then l2 else l1
    u' = if u1 == infinity then u2 else u1

-- | Abstract arithmetic operations
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
divInterval _ (Interval l2 u2)
  | l2 <= 0 && u2 >= 0 = Bottom  -- Division by zero possible
divInterval (Interval l1 u1) (Interval l2 u2)
  | l2 > 0 = Interval (l1 / u2) (u1 / l2)  -- Positive divisor
  | u2 < 0 = Interval (u1 / u2) (l1 / l2)  -- Negative divisor

-- ===== Abstract Environment =====

-- | Environment maps variables to abstract values
type Env = Map.Map String Interval

-- | Initial empty environment
emptyEnv :: Env
emptyEnv = Map.empty

-- | Look up a variable in an environment
lookupVar :: String -> Env -> Interval
lookupVar var env = fromMaybe Bottom (Map.lookup var env)

-- | Update a variable in an environment
updateVar :: String -> Interval -> Env -> Env
updateVar = Map.insert

-- | Join two environments
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

-- | A small imperative language
data Expr
  = Lit Double               -- Literal value
  | Var String               -- Variable reference
  | Add Expr Expr            -- Addition
  | Sub Expr Expr            -- Subtraction
  | Mul Expr Expr            -- Multiplication
  | Div Expr Expr            -- Division
  | Neg Expr                 -- Negation
  deriving Show

data BoolExpr
  = Lt Expr Expr             -- Less than
  | Lte Expr Expr            -- Less than or equal
  | Gt Expr Expr             -- Greater than
  | Gte Expr Expr            -- Greater than or equal
  | Eq Expr Expr             -- Equal
  | Neq Expr Expr            -- Not equal
  | And BoolExpr BoolExpr    -- Logical and
  | Or BoolExpr BoolExpr     -- Logical or
  | Not BoolExpr             -- Logical not
  deriving Show

data Stmt
  = Assign String Expr       -- Assignment
  | If BoolExpr Stmt Stmt    -- Conditional
  | While BoolExpr Stmt      -- Loop
  | Seq [Stmt]               -- Sequence of statements
  | Skip                     -- No operation
  deriving Show

-- ===== Abstract Interpreter =====

-- | Evaluate an expression in an abstract environment
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

-- | Evaluate a boolean expression to abstract boolean domain (True, False, Maybe)
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

-- | Filter an environment based on a boolean expression
filterEnv :: BoolExpr -> Env -> Env -> (Env, Env)
filterEnv (Lt (Var x) (Lit n)) env _ =
  case lookupVar x env of
    Bottom -> (env, env)
    Interval l u ->
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
    ABMaybe -> (env, env)  -- Over-approximation, could be refined

-- | Interpret a statement in an abstract environment
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
  | k <= 0 = top  -- Reached iteration limit, return top (over-approximation)
  | otherwise =
      let (trueEnv, falseEnv) = filterEnv cond env env
          bodyResult = interpretStmt body trueEnv (k-1)
          fixpoint = fixpointIteration w env bodyResult (k-1)
          (_, finalFalseEnv) = filterEnv cond fixpoint fixpoint
      in finalFalseEnv

-- | Perform fixpoint iteration for loops with widening/narrowing
fixpointIteration :: Stmt -> Env -> Env -> Int -> Env
fixpointIteration (While cond body) prevEnv currEnv k
  | k <= 0 = joinEnv prevEnv currEnv  -- Iteration limit reached
  | otherwise =
      let widenedEnv = widenEnv prevEnv currEnv
          (trueEnv, _) = filterEnv cond widenedEnv widenedEnv
          
          -- If trueEnv is bottom, we've reached a fixpoint
          newEnv = if allBottom trueEnv 
                   then widenedEnv 
                   else interpretStmt body trueEnv k
          
          -- Check if we've reached a fixpoint
          isFixpoint = envSubset newEnv widenedEnv
      in if isFixpoint
         then narrowEnv widenedEnv newEnv
         else fixpointIteration (While cond body) widenedEnv (joinEnv widenedEnv newEnv) (k-1)

-- | Check if an environment contains only bottom values
allBottom :: Env -> Bool
allBottom env = all (== Bottom) (Map.elems env)

-- | Check if one environment is a subset of another
envSubset :: Env -> Env -> Bool
envSubset env1 env2 =
  all (\(var, val1) -> 
    let val2 = lookupVar var env2
    in intervalSubset val1 val2) (Map.toList env1)

-- | Check if one interval is a subset of another
intervalSubset :: Interval -> Interval -> Bool
intervalSubset Bottom _ = True
intervalSubset _ Bottom = False
intervalSubset (Interval l1 u1) (Interval l2 u2) = l2 <= l1 && u1 <= u2

-- | Apply widening to each variable in the environment
widenEnv :: Env -> Env -> Env
widenEnv env1 env2 =
  let allVars = Map.keys env1 ++ Map.keys env2
      uniqueVars = foldr (\x acc -> if x `elem` acc then acc else x:acc) [] allVars
  in foldr (\var acc -> 
      let val1 = lookupVar var env1
          val2 = lookupVar var env2
      in updateVar var (widen val1 val2) acc) 
    emptyEnv uniqueVars

-- | Apply narrowing to each variable in the environment
narrowEnv :: Env -> Env -> Env
narrowEnv env1 env2 =
  let allVars = Map.keys env1 ++ Map.keys env2
      uniqueVars = foldr (\x acc -> if x `elem` acc then acc else x:acc) [] allVars
  in foldr (\var acc -> 
      let val1 = lookupVar var env1
          val2 = lookupVar var env2
      in updateVar var (narrow val1 val2) acc) 
    emptyEnv uniqueVars

-- | Main abstract interpretation function
abstractInterpret :: Stmt -> Env -> Env
abstractInterpret stmt initialEnv = interpretStmt stmt initialEnv 100  -- Limit to 100 iterations

-- ===== Example Usage =====

-- | Example program: compute Fibonacci numbers
fibonacciProgram :: Stmt
fibonacciProgram = Seq
  [ Assign "n" (Lit 10)               -- Input: compute 10th Fibonacci number
  , Assign "a" (Lit 0)                -- Initialize a = 0
  , Assign "b" (Lit 1)                -- Initialize b = 1
  , Assign "i" (Lit 0)                -- Initialize counter i = 0
  , While (Lt (Var "i") (Var "n"))    -- While i < n
      (Seq
        [ Assign "temp" (Var "a")     -- temp = a
        , Assign "a" (Var "b")        -- a = b
        , Assign "b" (Add (Var "temp") (Var "b"))  -- b = temp + b
        , Assign "i" (Add (Var "i") (Lit 1))      -- i = i + 1
        ]
      )
  ]

-- | Run the example
runExample :: IO ()
runExample = do
  let initialEnv = emptyEnv
      resultEnv = abstractInterpret fibonacciProgram initialEnv
  
  putStrLn "Abstract Interpretation of Fibonacci program:"
  putStrLn "--------------------------------------------"
  mapM_ (\(var, val) -> putStrLn $ var ++ " = " ++ show val) (Map.toList resultEnv)
  putStrLn "--------------------------------------------"
  putStrLn "The result of Fibonacci(10) is in variable 'a'"

-- | Main function
main :: IO ()
main = runExample
