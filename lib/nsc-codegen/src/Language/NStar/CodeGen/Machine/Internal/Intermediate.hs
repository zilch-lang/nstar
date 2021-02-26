{-# LANGUAGE BangPatterns #-}

module Language.NStar.CodeGen.Machine.Internal.Intermediate
( InterOpcode(..)
, TypeContext
) where

import Data.Word (Word8)
import Data.Text (Text)
import Data.Map (Map)
import Data.Located (Located)
import Language.NStar.Typechecker.Core (Type, Register)

-- we need an IR where labels still exist, but everything else is already compiled.
-- that way, we can handle forward and backward jumps easily.

-- ^ Represents an intermediate instruction, where jumps and labels are explicit.
data InterOpcode
  -- | Any byte
  = Byte !Word8
  -- | A label (jump destination)
  | Label !Text
  -- | A jump to a given label
  | Jump !Text
  -- | A data access through a label (disp32)
  | Symbol32 !Text
      Integer -- ^ The offset from the label
  -- | The address of a label (imm64)
  | Symbol64 !Text
  deriving (Show)

type TypeContext = (Map (Located Register) (Located Type), Located Type, Located Type)
