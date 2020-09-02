module Data.Located
( module Text.Diagnose.Position
, Located(..)
, unLoc
) where

import Text.Diagnose.Position

-- | A simple data type to hold some location to keep track off.
data Located a
  = a :@ Position
  deriving (Show)

infix 3 :@

instance Eq (Located a) where
  (_ :@ p1) == (_ :@ p2) = p1 == p2

instance Ord (Located a) where
  (_ :@ p1) <= (_ :@ p2) = p1 <= p2

-- | Removes extra position information bundled with a value.
unLoc :: Located a -> a
unLoc ~(x :@ _) = x
