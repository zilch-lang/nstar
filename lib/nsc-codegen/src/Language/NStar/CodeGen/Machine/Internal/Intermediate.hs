{-# LANGUAGE BangPatterns #-}

module Language.NStar.CodeGen.Machine.Internal.Intermediate
( InterOpcode(..)
) where

import Data.Word (Word8)
import Data.Text (Text)

-- we need an IR where labels still exist, but everything else is already compiled.
-- that way, we can handle forward and backward jumps easily.

-- ^ Represents an intermediate instruction, where jumps and labels are explicit.
data InterOpcode
  -- | Any byte
  = Byte !Word8
  -- | A label (jump destination)
  | Label !Text
  -- | An unconditional jump to a given label
  | Jump !Text
