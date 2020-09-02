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

instance Functor Located where
  fmap f (a :@ p) = f a :@ p

instance Applicative Located where
  pure x = x :@ Position (1, 1) (1, 1) "<unknown>"
  (f :@ _) <*> (x :@ p) = f x :@ p
