module Data.Located where

import Text.Diagnose.Position

-- | A simple data type to hold some locatio to keep track off.
data Located a
  = a :@ Position
  deriving (Show)

infix 3 :@

instance Eq (Located a) where
  (_ :@ p1) == (_ :@ p2) = p1 == p2

instance Ord (Located a) where
  (_ :@ p1) <= (_ :@ p2) = p1 <= p2
