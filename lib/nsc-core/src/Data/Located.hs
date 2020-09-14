module Data.Located
( module Text.Diagnose.Position
, Located(..)
, unLoc, getPos
) where

import Text.Diagnose.Position

-- | A simple data type to hold some location to keep track off.
data Located a
  = a :@ Position
  deriving (Show)

infix 3 :@

instance Eq a => Eq (Located a) where
  (v1 :@ _) == (v2 :@ _) = v1 == v2

instance Ord a => Ord (Located a) where
  (v1 :@ _) <= (v2 :@ _) = v1 <= v2

-- | Removes extra position information bundled with a value.
unLoc :: Located a -> a
unLoc ~(x :@ _) = x

-- | Retrieves the position from some 'Located' data.
getPos :: Located a -> Position
getPos ~(_ :@ p) = p

instance Functor Located where
  fmap f (a :@ p) = f a :@ p

instance Applicative Located where
  pure x = x :@ Position (1, 1) (1, 1) "<unknown>"
  (f :@ _) <*> (x :@ p) = f x :@ p
