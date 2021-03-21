{-# LANGUAGE GADTs #-}

module Data.Elf.Internal.Endianness where

import Data.Kind (Type)

-- | Endianness order
data Order
  = L   -- ^ little
  | B   -- ^ big

data Endianness :: Order -> Type where
  LE :: Endianness 'L    -- ^ little endian
  BE :: Endianness 'B    -- ^ big endian
