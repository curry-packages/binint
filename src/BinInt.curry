-----------------------------------------------------------------------------
--- This library contains data types for a binary representation of
--- natural and integers.
--- It is based on the paper _Declaring Numbers_ by Brassel/Fischer/Huch
--- [ENTS 216, 2008](http://dx.doi.org/10.1016/j.entcs.2008.06.037).
---
--- The advantage of this algebraic definition in contrast to built-in
--- integers is the possibility to use them also for narrowing, i.e.,
--- functional logic programming. Since the operations are also quite
--- efficient compared to a simple Peano representations, this library
--- is also the basis of the implementation of integers in the Curry
--- implementation [KiCS2](http://www-ps.informatik.uni-kiel.de/kics2/).

--- @version February 2017
-----------------------------------------------------------------------------

module BinInt where


-----------------------------------------------------------------------------
-- Nat
-----------------------------------------------------------------------------

--- Algebraic data type to represent natural numbers.
--- @cons IHi - most significant bit
--- @cons O   - multiply with 2
--- @cons I   - multiply with 2 and add 1
data Nat = IHi | O Nat | I Nat

--- Successor, O(n)
succ :: Nat -> Nat
succ IHi    = O IHi        -- 1       + 1 = 2
succ (O bs) = I bs         -- 2*n     + 1 = 2*n + 1
succ (I bs) = O (succ bs)  -- 2*n + 1 + 1 = 2*(n+1)

--- Predecessor, O(n)
pred :: Nat -> Nat
pred IHi         = failed     -- 1 has no predecessor
pred (O IHi)     = IHi        -- 2           - 1 = 1
pred (O x@(O _)) = I (pred x) -- 2*2*n       - 1 = 2*(2*n-1) + 1
pred (O (I x))   = I (O x)    -- 2*(2*n + 1) - 1 = 2*2*n + 1
pred (I x)       = O x        -- 2*n + 1      -1 = 2*n

--- Addition, O(max (m, n))
(+^) :: Nat -> Nat -> Nat
IHi +^ y   = succ y           -- 1  +  n   = n + 1
O x +^ IHi = I x              -- 2*n + 1   = 2*n + 1
O x +^ O y = O (x +^ y)       -- 2*m + 2*n = 2*(m+n)
O x +^ I y = I (x +^ y)
I x +^ IHi = O (succ x)
I x +^ O y = I (x +^ y)
I x +^ I y = O (succ x +^ y)

--- Subtraction
(-^) :: Nat -> Nat -> BinInt
IHi     -^ y     = inc (Neg y)           -- 1-n = 1+(-n)
x@(O _) -^ IHi   = Pos (pred x)          --
(O x)   -^ (O y) = mult2 (x -^ y)
(O x)   -^ (I y) = dec (mult2 (x -^ y))
(I x)   -^ IHi   = Pos (O x)
(I x)   -^ (O y) = inc (mult2 (x -^ y))  -- 2*n+1 - 2*m = 1+2*(n-m)
(I x)   -^ (I y) = mult2 (x -^ y)        -- 2*n+1 - (2*m+1) = 2*(n-m)

--- Multiplication by 2
mult2 :: BinInt -> BinInt
mult2 (Pos n) = Pos (O n)
mult2 Zero    = Zero
mult2 (Neg n) = Neg (O n)

--- Multiplication, O(m*n)
(*^) :: Nat -> Nat -> Nat
IHi   *^ y = y
(O x) *^ y = O (x *^ y)
(I x) *^ y = y +^ (O (x *^ y))
-- (I x) *^ IHi = I x
-- (I x) *^ (O y) = (O y) +^ (O (x *^ (O y))) = O (y +^ (x *^ (O y)))
-- (I x) *^ (I y) = (I y) +^ (O (x *^ (I y))) = I (y +^ (x *^ (I y)))

--- Division by 2
div2 :: Nat -> Nat
div2 IHi   = failed -- 1 div 2 is not defined for Nat
div2 (O x) = x
div2 (I x) = x

--- Modulo by 2
mod2 :: Nat -> BinInt
mod2 IHi   = Pos IHi
mod2 (O _) = Zero
mod2 (I _) = Pos IHi

--- Comparison of natural numbers, O(min (m,n))
cmpNat :: Nat -> Nat -> Ordering
cmpNat IHi   IHi   = EQ
cmpNat IHi   (O _) = LT
cmpNat IHi   (I _) = LT
cmpNat (O _) IHi   = GT
cmpNat (O x) (O y) = cmpNat x y
cmpNat (O x) (I y) = case cmpNat x y of
  EQ    -> LT
  cmpxy -> cmpxy
cmpNat (I _) IHi   = GT
cmpNat (I x) (O y) = case cmpNat x y of
  EQ    -> GT
  cmpxy -> cmpxy
cmpNat (I x) (I y) = cmpNat x y

--- Quotient and remainder.
quotRemNat :: Nat -> Nat -> (BinInt, BinInt)
quotRemNat x y
  | y == IHi  = (Pos x, Zero   ) -- quotRemNat x 1 = (x, 0)
  | x == IHi  = (Zero , Pos IHi) -- quotRemNat 1 y = (0, 1)
  | otherwise = case cmpNat x y of
      EQ -> (Pos IHi, Zero )   -- x = y : quotRemNat x y = (1, 0)
      LT -> (Zero   , Pos x)   -- x < y : quotRemNat x y = (0, x)
      GT -> case quotRemNat (div2 x) y of
        (Neg _, _    ) -> error "quotRemNat: negative quotient"
        (Zero , _    ) -> (Pos IHi  , x -^ y) -- x > y, x/2 < y  : quotRemNat x y = (1, x - y)
        (Pos _, Neg _) -> error "quotRemNat: negative remainder"
        (Pos d, Zero ) -> (Pos (O d), mod2 x)
        (Pos d, Pos m) -> case quotRemNat (shift x m) y of
          (Neg _ , _ ) -> error "quotRemNat: negative quotient"
          (Zero  , m') -> (Pos (O d)      , m')
          (Pos d', m') -> (Pos (O d +^ d'), m')
  where
    shift IHi   _ = error "quotRemNat.shift: IHi"
    shift (O _) n = O n
    shift (I _) n = I n

-----------------------------------------------------------------------------
-- Integer
-----------------------------------------------------------------------------

--- Algebraic data type to represent integers.
data BinInt = Neg Nat | Zero | Pos Nat

--- Less-than-or-equal on BinInt
lteqInteger :: BinInt -> BinInt -> Bool
lteqInteger x y = cmpInteger x y /= GT

--- Comparison on BinInt, O(min (m, n))
cmpInteger :: BinInt -> BinInt -> Ordering
cmpInteger Zero    Zero    = EQ
cmpInteger Zero    (Pos _) = LT
cmpInteger Zero    (Neg _) = GT
cmpInteger (Pos _) Zero    = GT
cmpInteger (Pos x) (Pos y) = cmpNat x y
cmpInteger (Pos _) (Neg _) = GT
cmpInteger (Neg _) Zero    = LT
cmpInteger (Neg _) (Pos _) = LT
cmpInteger (Neg x) (Neg y) = cmpNat y x

--- Unary minus. Usually written as "- e".
neg :: BinInt -> BinInt
neg Zero    = Zero
neg (Pos x) = Neg x
neg (Neg x) = Pos x

--- Increment
inc :: BinInt -> BinInt
inc Zero        = Pos IHi
inc (Pos n)     = Pos (succ n)
inc (Neg IHi)   = Zero
inc (Neg (O n)) = Neg (pred (O n))
inc (Neg (I n)) = Neg (O n)

--- Decrement
dec :: BinInt -> BinInt
dec Zero        = Neg IHi
dec (Pos IHi)   = Zero
dec (Pos (O n)) = Pos (pred (O n))
dec (Pos (I n)) = Pos (O n)
dec (Neg n)     = Neg (succ n)

--- Adds two BinInts.
(+#)   :: BinInt -> BinInt -> BinInt
Zero      +# x     = x
x@(Pos _) +# Zero  = x
Pos x     +# Pos y = Pos (x +^ y)
Pos x     +# Neg y = x -^ y
x@(Neg _) +# Zero  = x
Neg x     +# Pos y = y -^ x
Neg x     +# Neg y = Neg (x +^ y)

--- Subtracts two BinInts.
(-#)   :: BinInt -> BinInt -> BinInt
x -# Zero  = x
x -# Pos y = x +# Neg y
x -# Neg y = x +# Pos y

--- Multiplies two BinInts.
(*#)   :: BinInt -> BinInt -> BinInt
Zero  *# _     = Zero
Pos _ *# Zero  = Zero
Pos x *# Pos y = Pos (x *^ y)
Pos x *# Neg y = Neg (x *^ y)
Neg _ *# Zero  = Zero
Neg x *# Pos y = Neg (x *^ y)
Neg x *# Neg y = Pos (x *^ y)

--- Quotient and Remainder, truncated against zero
quotRemInteger :: BinInt -> BinInt -> (BinInt, BinInt)
quotRemInteger _       Zero    = failed -- division by zero is not defined
quotRemInteger Zero    (Pos _) = (Zero, Zero)
quotRemInteger Zero    (Neg _) = (Zero, Zero)
quotRemInteger (Pos x) (Pos y) = quotRemNat x y
quotRemInteger (Neg x) (Pos y) = let (d, m) = quotRemNat x y in (neg d, neg m)
quotRemInteger (Pos x) (Neg y) = let (d, m) = quotRemNat x y in (neg d,     m)
quotRemInteger (Neg x) (Neg y) = let (d, m) = quotRemNat x y in (d    , neg m)

--- Quotient and Remainder, truncated against negative infinity
divModInteger :: BinInt -> BinInt -> (BinInt, BinInt)
divModInteger _       Zero    = failed -- division by zero is not defined
divModInteger Zero    (Pos _) = (Zero, Zero)
divModInteger Zero    (Neg _) = (Zero, Zero)
divModInteger (Pos x) (Pos y) = quotRemNat x y
divModInteger (Neg x) (Pos y) = let (d, m) = quotRemNat x y in case m of
  Zero -> (neg d, m)
  _    -> (neg (inc d), (Pos y) -# m)
divModInteger (Pos x) (Neg y) = let (d, m) = quotRemNat x y in case m of
  Zero -> (neg d, m)
  _    -> (neg (inc d), m -# (Pos y))
divModInteger (Neg x) (Neg y) = let (d, m) = quotRemNat x y in (d, neg m)

--- Integer divisor, truncated towards negative infinity.
divInteger :: BinInt -> BinInt -> BinInt
divInteger x y = fst (divModInteger x y)

--- Integer modulo, truncated towards negative infinity.
modInteger :: BinInt -> BinInt -> BinInt
modInteger x y = snd (divModInteger x y)

--- Integer quotient, truncated towards zero.
quotInteger :: BinInt -> BinInt -> BinInt
quotInteger x y = fst (quotRemInteger x y)

--- Integer remainder, truncated towards zero.
remInteger :: BinInt -> BinInt -> BinInt
remInteger x y = snd (quotRemInteger x y)

-----------------------------------------------------------------------------
