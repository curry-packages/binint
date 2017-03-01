-----------------------------------------------------------------------------
--- This library contains operations to convert binary numbers into
--- standard integer constants.
---
--- @version February 2017
-----------------------------------------------------------------------------

module ConvertBinInt where

import BinInt

--- Converts a binary natural number into an integer constant.
fromNat :: Nat -> Int
fromNat IHi = 1
fromNat (O n) = 2 * fromNat n
fromNat (I n) = 2 * fromNat n + 1

--- Converts a binary integer into an integer constant.
fromBinInt :: BinInt -> Int
fromBinInt (Neg n) = - (fromNat n)
fromBinInt Zero    = 0
fromBinInt (Pos n) = fromNat n

-----------------------------------------------------------------------------

