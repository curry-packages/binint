module TestBinInt where

import Prelude hiding (pred,succ)
import Test.Prop

import Data.BinInt

-----------------------------------------------------------------------------
-- Test cases to check the transformations of representation.
-----------------------------------------------------------------------------

-- Property: transforming positive integers into natural numbers and back
-- is the identity.
toFromNat :: Int -> Prop
toFromNat n = n>0 ==> fromNat (toNat n) -=- n

-- Property: transforming natural numbers into integers and back
-- is the identity.
fromToNat :: Nat -> Prop
fromToNat n = toNat (fromNat n) -=- n

-- Property: transforming integers into binary numbers and back
-- is the identity.
toFromBinInt :: Int -> Prop
toFromBinInt n = fromBinInt (toBinInt n) -=- n

-- Property: transforming binary numbers into integers and back
-- is the identity
fromToBinInt :: BinInt -> Prop
fromToBinInt n = toBinInt (fromBinInt n) -=- n

-----------------------------------------------------------------------------
-- Test cases to check the binary representation against the built-in integer
-- operations.
-----------------------------------------------------------------------------

test_cmpNat :: Nat -> Nat -> Prop
test_cmpNat x y = cmpNat x y -=- compare (fromNat x) (fromNat y)

test_succ :: Nat -> Prop
test_succ x = fromNat (succ x) -=- fromNat x + 1

test_pred :: Nat -> Prop
test_pred x = x /= IHi ==> fromNat (pred x) -=- fromNat x - 1

test_addNat :: Nat -> Nat -> Prop
test_addNat x y = fromNat (x +^ y) -=- fromNat x + fromNat y

test_subNat :: Nat -> Nat -> Prop
test_subNat x y = fromBinInt (x -^ y) -=- fromNat x - fromNat y

test_mult2 :: BinInt -> Prop
test_mult2 x = fromBinInt (mult2 x) -=- fromBinInt x * 2

test_multNat :: Nat -> Nat -> Prop
test_multNat x y = fromNat (x *^ y) -=- fromNat x * fromNat y

test_div2 :: Nat -> Prop
test_div2 x = x /= IHi ==> fromNat (div2 x) -=- fromNat x `div` 2

test_mod2 :: Nat -> Prop
test_mod2 x = fromBinInt (mod2 x) -=- fromNat x `mod` 2

test_quotRemNat :: Nat -> Nat -> Prop
test_quotRemNat x y =
  let (q, r) = quotRemNat x y
  in  (fromBinInt q, fromBinInt r) -=- quotRem (fromNat x) (fromNat y)

test_lteqInteger :: BinInt -> BinInt -> Prop
test_lteqInteger x y = lteqInteger x y -=- fromBinInt x <= fromBinInt y

test_cmpInteger :: BinInt -> BinInt -> Prop
test_cmpInteger x y = cmpInteger x y -=- fromBinInt x `compare` fromBinInt y

test_neg :: BinInt -> Prop
test_neg x = fromBinInt (neg x) -=- - (fromBinInt x)

test_inc :: BinInt -> Prop
test_inc x = fromBinInt (inc x) -=- fromBinInt x + 1

test_dec :: BinInt -> Prop
test_dec x = fromBinInt (dec x) -=- fromBinInt x - 1

test_add :: BinInt -> BinInt -> Prop
test_add x y = fromBinInt (x +# y) -=- fromBinInt x + fromBinInt y

test_sub :: BinInt -> BinInt -> Prop
test_sub x y = fromBinInt (x -# y) -=- fromBinInt x - fromBinInt y

test_mult :: BinInt -> BinInt -> Prop
test_mult x y = fromBinInt (x *# y) -=- fromBinInt x * fromBinInt y

test_quotRem :: BinInt -> BinInt -> Prop
test_quotRem x y
  = y /= Zero ==>
    let (q, r) = quotRemInteger x y
    in  (fromBinInt q, fromBinInt r) -=- quotRem (fromBinInt x) (fromBinInt y)

test_divMod :: BinInt -> BinInt -> Prop
test_divMod x y
  = y /= Zero ==>
    let (d, m) = divModInteger x y
    in  (fromBinInt d, fromBinInt m) -=- divMod (fromBinInt x) (fromBinInt y)

test_div :: BinInt -> BinInt -> Prop
test_div x y
  = y /= Zero ==>
    fromBinInt (divInteger x y) -=- div (fromBinInt x) (fromBinInt y)

test_mod :: BinInt -> BinInt -> Prop
test_mod x y
  = y /= Zero ==>
    fromBinInt (modInteger x y) -=- mod (fromBinInt x) (fromBinInt y)

test_quot :: BinInt -> BinInt -> Prop
test_quot x y
  = y /= Zero ==>
    fromBinInt (quotInteger x y) -=- quot (fromBinInt x) (fromBinInt y)

test_rem :: BinInt -> BinInt -> Prop
test_rem x y
  = y /= Zero ==>
    fromBinInt (remInteger x y) -=- rem (fromBinInt x) (fromBinInt y)

-----------------------------------------------------------------------------
-- Test cases for narrowing on binary numbers.
-----------------------------------------------------------------------------

solveAddFour :: (BinInt,BinInt)
solveAddFour = Pos x + Pos y =:= 4 &> (Pos x,Pos y)
 where x,y free

testSolveAddFour :: Prop
testSolveAddFour = solveAddFour <~> ( (1,3) ? (2,2) ? (3,1))

-----------------------------------------------------------------------------
