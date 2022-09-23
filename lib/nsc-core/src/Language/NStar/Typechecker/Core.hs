{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

-- |
--  Module: Language.NStar.Typechecker.Core
--  Description: NStar's typechecking core language
--  Copyright: (c) Mesabloo, 2020
--  License: BSD3
--  Stability: experimental
module Language.NStar.Typechecker.Core
  ( TypedProgram (..),
    TypedDataSection (..),
    TypedRODataSection (..),
    TypedUDataSection (..),
    TypedCodeSection (..),
    TypedExternCodeSection (..),
    TypedStatement (..),
    TypedInstruction (..),

    -- * Re-exports
    module Language.NStar.Syntax.Core,
  )
where

import Data.Located (Located)
import Data.Map (Map)
import Data.Text (Text)
import Language.NStar.Syntax.Core (Binding (..), Expr (..), Kind (..), Register (..), ReservedSpace (..), Type (..))

data TypedProgram
  = TProgram
      (Located TypedDataSection)
      (Located TypedRODataSection)
      (Located TypedUDataSection)
      (Located TypedCodeSection)
      (Located TypedExternCodeSection)

data TypedDataSection where
  TData ::
    [Located Binding] ->
    TypedDataSection

data TypedRODataSection where
  TROData ::
    [()] ->
    TypedRODataSection

data TypedUDataSection where
  TUData ::
    [()] ->
    TypedUDataSection

data TypedCodeSection where
  TCode ::
    [Located TypedStatement] ->
    TypedCodeSection

data TypedExternCodeSection where
  TExternCode ::
    [Located ReservedSpace] ->
    TypedExternCodeSection

data TypedStatement where
  -- | A label stripped off its context.
  TLabel ::
    -- | the name of the label
    Located Text ->
    -- | Label's scope
    [TypedStatement] ->
    TypedStatement
  -- | An instruction with type information attached to it.
  TInstr ::
    -- | the typed instruction
    Located TypedInstruction ->
    Map (Located Register) (Located Type) ->
    Located Type ->
    Located Type ->
    TypedStatement

deriving instance Show TypedStatement

-- | The core of the N* abstract machine.
data TypedInstruction where
  -- | Transfers the control flow to the code address pointed by @l@.
  JMP ::
    -- | > l
    Located Expr ->
    TypedInstruction
  -- | Does absolutely nothing.
  NOP :: TypedInstruction
  -- | Moves a value @s@ (either a literal value or a value taken from a register) into the destination register @d@
  MV ::
    -- | > s
    Located Expr ->
    -- | > d
    Located Register ->
    TypedInstruction
  -- | Allocates a space of @n@ bytes on top of the stack.
  SALLOC ::
    -- | > n
    Located Integer ->
    TypedInstruction
  -- | Pops a space of @n@ bytes off the top of the stack.
  SFREE ::
    -- | > n
    Located Integer ->
    TypedInstruction
  -- | Copies @s@ (@8@) bytes taken @n@ bytes from the top of the stack into the register @r@.
  SLD ::
    -- | > n
    --  -> Located Integer   -- ^ > s
    Located Integer ->
    -- | > r
    Located Register ->
    TypedInstruction
  -- | Copies the value in the register @r@ to @n@ bytes from the top of the stack.
  SST ::
    -- | > r
    Located Expr ->
    -- | > n
    Located Integer ->
    TypedInstruction
  -- | Copies @s@ (@8@) bytes at the address @p + o@ into the register @r@.
  LD ::
    -- | > o
    Located Expr ->
    -- | > p
    -- -> Located Integer   -- ^ > s
    Located Expr ->
    -- | > r
    Located Register ->
    TypedInstruction
  -- | Copies the value in the register @r@ into the memory space at the address @p + o@.
  ST ::
    -- | > r
    Located Expr ->
    -- | > o
    Located Expr ->
    -- | > p
    Located Expr ->
    TypedInstruction
  -- | Retrieves a pointer to @p@ bytes long data located at @n@ bytes onto the stack in the register @r@.
  SREF ::
    -- | > n
    Located Integer ->
    -- | > p
    Located Integer ->
    -- | > r
    Located Register ->
    TypedInstruction
  -- | Performs bitwise AND on @x@ and @y@ and store result in register @s@.
  AND ::
    -- | > x
    Located Expr ->
    -- | > y
    Located Expr ->
    -- | > s
    Located Register ->
    TypedInstruction
  -- | Performs bitwise OR on @x@ and @y@ and store result in register @s@.
  OR ::
    -- | > x
    Located Expr ->
    -- | > y
    Located Expr ->
    -- | > s
    Located Register ->
    TypedInstruction
  -- | Performs bitwise XOR on @x@ and @y@ and store result in register @s@.
  XOR ::
    -- | > x
    Located Expr ->
    -- | > y
    Located Expr ->
    -- | > s
    Located Register ->
    TypedInstruction
  -- | Performs one's complement negation (bit reversing) on @e@ and return result in register @r@.
  NOT ::
    -- | > e
    Located Expr ->
    -- | > r
    Located Register ->
    TypedInstruction
  -- | Moves @b@ inside @r@ if @a@ is @0@, otherwise move @c@ inside @r@.
  CMVZ ::
    -- | > a
    Located Expr ->
    -- | > b
    Located Expr ->
    -- | > c
    Located Expr ->
    -- | > r
    Located Register ->
    TypedInstruction
  -- | Stores the result of @a + b@ in @r@.
  ADD ::
    -- | > a
    Located Expr ->
    -- | > b
    Located Expr ->
    -- | > r
    Located Register ->
    TypedInstruction

deriving instance Show TypedInstruction
