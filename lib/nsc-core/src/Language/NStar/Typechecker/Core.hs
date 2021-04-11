{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

{-|
  Module: Language.NStar.Typechecker.Core
  Description: NStar's typechecking core language
  Copyright: (c) Mesabloo, 2020
  License: BSD3
  Stability: experimental
-}

module Language.NStar.Typechecker.Core
(
  TypedProgram(..)
, TypedDataSection(..), TypedRODataSection(..), TypedUDataSection(..), TypedCodeSection(..), TypedExternCodeSection(..)
, TypedStatement(..)
, TypedInstruction(..)
,  -- * Re-exports
  module Language.NStar.Syntax.Core
) where

import Language.NStar.Syntax.Core (Expr(..), Type(..), Kind(..), Register(..), Binding(..), ReservedSpace(..))
import Data.Located (Located)
import Data.Text (Text)
import Data.Map (Map)

data TypedProgram =
  TProgram
    (Located TypedDataSection)
    (Located TypedRODataSection)
    (Located TypedUDataSection)
    (Located TypedCodeSection)
    (Located TypedExternCodeSection)

data TypedDataSection where
  TData :: [Located Binding]
        -> TypedDataSection

data TypedRODataSection where
  TROData :: [()]
          -> TypedRODataSection

data TypedUDataSection where
  TUData :: [()]
         -> TypedUDataSection

data TypedCodeSection where
  TCode :: [Located TypedStatement]
        -> TypedCodeSection

data TypedExternCodeSection where
  TExternCode :: [Located ReservedSpace]
              -> TypedExternCodeSection

data TypedStatement where
  -- | A label stripped off its context.
  TLabel :: Located Text         -- ^ the name of the label
         -> [TypedStatement]     -- ^ Label's scope
         -> TypedStatement
  -- | An instruction with type information attached to it.
  TInstr :: Located TypedInstruction  -- ^ the typed instruction
         -> Map (Located Register) (Located Type)
         -> Located Type
         -> Located Type
         -> TypedStatement

deriving instance Show TypedStatement

-- | The core of the N* abstract machine.
data TypedInstruction where
  -- | Transfers the control flow to the code address pointed by @l@.
  JMP :: Located Expr      -- ^ > l
      -> TypedInstruction
  -- | Does absolutely nothing.
  NOP :: TypedInstruction
  -- | Moves a value @s@ (either a literal value or a value taken from a register) into the destination register @d@
  MV :: Located Expr       -- ^ > s
     -> Located Register   -- ^ > d
     -> TypedInstruction
  -- | Allocates a space of @n@ bytes on top of the stack.
  SALLOC :: Located Integer   -- ^ > n
         -> TypedInstruction
  -- | Pops a space of @n@ bytes off the top of the stack.
  SFREE :: Located Integer    -- ^ > n
        -> TypedInstruction
  -- | Copies @s@ (@8@) bytes taken @n@ bytes from the top of the stack into the register @r@.
  SLD :: Located Integer   -- ^ > n
  --  -> Located Integer   -- ^ > s
      -> Located Register  -- ^ > r
      -> TypedInstruction
  -- | Copies the value in the register @r@ to @n@ bytes from the top of the stack.
  SST :: Located Expr     -- ^ > r
      -> Located Integer  -- ^ > n
      -> TypedInstruction
  -- | Copies @s@ (@8@) bytes at the address @p + o@ into the register @r@.
  LD :: Located Expr      -- ^ > o
     -> Located Expr      -- ^ > p
  -- -> Located Integer   -- ^ > s
     -> Located Register  -- ^ > r
     -> TypedInstruction
  -- | Copies the value in the register @r@ into the memory space at the address @p + o@.
  ST :: Located Expr     -- ^ > r
     -> Located Expr     -- ^ > o
     -> Located Expr     -- ^ > p
     -> TypedInstruction
  -- | Retrieves a pointer to @p@ bytes long data located at @n@ bytes onto the stack in the register @r@.
  SREF :: Located Integer   -- ^ > n
       -> Located Integer   -- ^ > p
       -> Located Register  -- ^ > r
       -> TypedInstruction

deriving instance Show TypedInstruction
