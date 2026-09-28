---------------------
--  SIGN ANALYSIS  --
---------------------
data Sign = Positive | Negative | Zero | Top | Bottom
  deriving (Eq, Show)

-- Join operation for signs
join :: Sign -> Sign -> Sign
join Bottom x = x
join x Bottom = x
join Top _ = Top
join _ Top = Top
join Positive Positive = Positive
join Negative Negative = Negative
join Zero Zero = Zero
join _ _ = Top  -- When mixing different non-bottom signs

-- Abstract operations for sign analysis
addSign :: Sign -> Sign -> Sign
addSign Bottom _ = Bottom
addSign _ Bottom = Bottom
addSign Positive Positive = Positive
addSign Negative Negative = Negative
addSign Zero x = x
addSign x Zero = x
addSign Positive Negative = Top  -- Could be either positive, negative or zero
addSign Negative Positive = Top


-----------------------
--  PARITY ANALYSIS  --
-----------------------
data Parity = Even | Odd | Bottom | Top
  deriving (Eq, Show)

-- Join operation for parity
join :: Parity -> Parity -> Parity
join Bottom x = x
join x Bottom = x
join Even Even = Even
join Odd Odd = Odd
join _ _ = Top

-- Abstract operations for parity analysis
addParity :: Parity -> Parity -> Parity
addParity Bottom _ = Bottom
addParity _ Bottom = Bottom
addParity Even Even = Even
addParity Odd Odd = Even
addParity Even Odd = Odd
addParity Odd Even = Odd


----------------------------
--  CONSTANT PROPAGATION  --
----------------------------
data Constant = Const Double | NotConst | Bottom
  deriving (Eq, Show)

-- Join operation for constants
join :: Constant -> Constant -> Constant
join Bottom x = x
join x Bottom = x
join (Const n) (Const m) 
  | n == m    = Const n
  | otherwise = NotConst
join _ _ = NotConst
