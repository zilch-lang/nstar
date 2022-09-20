{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

-- |
--  Module: Language.NStar.Syntax.Core
--  Description: NStar's syntactic core language
--  Copyright: (c) Mesabloo, 2020
--  License: BSD3
--  Stability: experimental
--
--  This module contains all the definitions of all AST nodes and Tokens that will be
--  used across the whole compiler.
--
--  The translation from the source code to the syntactic core should be almost a 1:1 conversion,
--  so a program can be printed back as it was. Both lexing and parsing steps must be reversible steps.
module Language.NStar.Syntax.Core where

import Data.Data (Data)
import Data.Located
import Data.Map (Map)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Numeric.Natural (Natural)

newtype Program
  = -- | A program is a (possibly empty) list of sections
    Program [Located Section]

deriving instance Show Program

data Section where
  -- | The @code@ section
  CodeS ::
    [Located Statement] ->
    Section
  -- | The @data@ section
  DataS ::
    [Located Binding] ->
    Section
  -- | The @rodata@ section
  RODataS ::
    [Located Binding] ->
    Section
  -- | The @udata@ section
  UDataS ::
    [Located ReservedSpace] ->
    Section
  -- | The @include@ section
  IncludeS ::
    [Located Text] ->
    Section
  -- | The @extern.code@ section
  ExternCodeS ::
    [Located ReservedSpace] ->
    Section

deriving instance Show Section

data Binding where
  -- | An initialized data binding
  Bind ::
    Located Text ->
    Located Type ->
    Located Constant ->
    Binding

deriving instance Show Binding

data ReservedSpace where
  -- | An uninitialized data binding (in the @udata@ section)
  ReservedBind ::
    Located Text ->
    Located Type ->
    ReservedSpace

deriving instance Show ReservedSpace

-- | A statement is either
data Statement where
  -- | A typed label
  Label ::
    -- | The label's name. It may not be empty
    Located Text ->
    -- | The "label's type", describing the minimal type expected when jumping to this label
    Located Type ->
    -- | Instructions contained in its block as well as the unsafetiness of each one
    [(Located Instruction, Bool)] ->
    Statement

deriving instance Show Statement

data Type where
  -- | Signed integer
  SignedT ::
    -- | The size of the integer (a multiple of 2 greater than 4)
    Natural ->
    Type
  -- | Unsigned integer
  UnsignedT ::
    -- | The size of the integer (a multiple of 2 greater than 4)
    Natural ->
    Type
  -- | Stack constructor
  ConsT ::
    -- | Stack head
    Located Type ->
    -- | Stack tail
    Located Type ->
    Type
  -- | Type variable
  VarT ::
    -- | The name of the type variable
    Located Text ->
    Type
  -- | Free type variable
  FVarT ::
    -- | The name of the type variable
    Located Text ->
    Type
  -- | Record type
  RecordT ::
    -- | A mapping from 'Register's to their expected 'Type's
    Map (Located Register) (Located Type) ->
    -- | The stack required on this context
    Located Type ->
    -- | The return continuation
    Located Type ->
    -- | Is the record opened or closed?
    Bool ->
    Type
  -- | Pointer to a normal type
  PtrT ::
    Located Type ->
    Type
  -- | Forall type variable binder
  ForAllT ::
    -- | Variables along with their 'Kind's
    [(Located Type, Located Kind)] ->
    Located Type ->
    Type
  -- | Register type
  RegisterT ::
    -- | Register size
    Natural ->
    Type
  -- | Stack continuation
  StackContT :: Integer -> Type
  -- | Register continuation
  RegisterContT :: Register -> Type
  -- | Bang type
  BangT :: Type
  -- | Packed structure type
  PackedStructT ::
    [Located Type] ->
    Type

deriving instance Show Type

deriving instance Eq Type

data Kind where
  -- | Kind of N-bytes big types
  T :: Integer -> Kind
  -- | Kind of stack types
  Ts :: Kind
  -- | Kind of unsized types
  Ta :: Kind
  -- | Kind of continuations
  Tc :: Kind

deriving instance Show Kind

deriving instance Eq Kind

data Register where
  -- | General purpose register
  R0, R1, R2, R3, R4, R5 :: Register

deriving instance Show Register

deriving instance Eq Register

deriving instance Ord Register

-- | N*'s instruction set
data Instruction where
  -- | @ret@ returns to the address on top of the stack.
  RET :: Instruction
  -- | @jmp@ alters the control flow by unconditionally jumping to the given address.
  JMP ::
    Located Expr ->
    Instruction
  -- | @call@ alters the control flow by pushing the current address onto the stack and jumping
  --   to the given address (either as a label or in a register).
  CALL ::
    Located Expr ->
    Instruction
  ADD ::
    -- | The source operand
    Located Expr ->
    -- | The increment value
    -- -> Located Register -- ^ The destination register
    Located Expr ->
    Instruction
  SUB ::
    Located Expr ->
    Located Expr ->
    -- -> Located Register -- ^ The destination register
    Instruction
  -- | Does strictly nothing. May be used as a padding instruction.
  NOP :: Instruction
  -- | Moves a literal or from a register into a register.
  MV ::
    Located Expr ->
    Located Register ->
    Instruction
  -- | Allocates some space on top of the stack.
  SALLOC ::
    Located Type ->
    Instruction
  -- | Frees the top-most stack cell.
  SFREE :: Instruction
  -- | Loads a value from the nth cell of the stack into a register.
  SLD ::
    Located Integer ->
    Located Register ->
    Instruction
  -- | Stores a literal value or from a register into an already-allocated stack cell.
  SST ::
    Located Expr ->
    Located Integer ->
    Instruction
  -- | Dereferences a pointer into a register.
  LD ::
    Located Expr ->
    Located Expr ->
    Instruction
  -- | Puts a value at a specific memory address.
  ST ::
    Located Expr ->
    Located Expr ->
    Instruction
  -- | Gets a pointer to some data on the stack.
  SREF ::
    Located Integer ->
    Located Register ->
    Instruction
  -- | Performs a bitwise AND on the first parameters and store the result in the third.
  AND ::
    -- | The first operand
    Located Expr ->
    -- | The second operand
    Located Expr ->
    -- | The destination of the computation
    Located Register ->
    Instruction

-- TODO: add more instructions

deriving instance Show Instruction

data Constant where
  -- | A constant integer
  IntegerC ::
    Located Integer ->
    Constant
  -- | A constant character
  CharacterC ::
    Located Char ->
    Constant
  -- | An array of constants
  ArrayC ::
    [Located Constant] ->
    Constant
  -- | A structure constant
  StructC ::
    [Located Constant] ->
    Constant

deriving instance Show Constant

data Expr where
  -- | An immediate value (@$⟨val⟩@)
  ImmE ::
    -- | \- @⟨val⟩@
    Located Immediate ->
    Expr
  -- | A label name with optional specialization (@〈label〉<〈type〉...>@)
  NameE ::
    Located Text ->
    [Located Type] ->
    Expr
  -- | A byte-offset expression (@〈offset〉(〈expr〉)@)
  ByteOffsetE ::
    -- | \- @⟨offset⟩@
    Located Expr ->
    -- | \- @⟨expr⟩@
    Located Expr ->
    Expr
  -- | A base-offset expression (@〈expr〉[〈offset〉]@)
  BaseOffsetE ::
    -- | \- @⟨expr⟩@
    Located Expr ->
    -- | \- @⟨offset⟩@
    Located Expr ->
    Expr
  -- | A register (one of the available 'Register's)
  RegE ::
    Located Register ->
    Expr

deriving instance Show Expr

data Immediate where
  -- | An integer, either in decimal, hexadecimal, octal or binary format
  --
  -- Grammars are:
  --
  -- * Binary: @0(b|B)(0|1)⁺@
  -- * Octal: @0(o|O)(0..7)⁺@
  -- * Decimal: @(0..9)⁺@
  -- * Hexadecimal: @0(x|X)(0..9|A..F|a..f)⁺@
  I :: Integer -> Immediate
  -- | A character (which can be an escape sequence)
  C :: Char -> Immediate

deriving instance Show Immediate

------------------------------------------------------------------------------------------------

data Token where
  -- Literals

  -- | A literal integer
  Integer :: Text -> Token
  -- | A literal character
  Char :: Char -> Token
  -- | An identifier (also called name)
  Id :: Text -> Token
  -- | A @"@ delimited string
  Str :: Text -> Token
  -- Registers

  -- | Registers reserved words
  R0', R1', R2', R3', R4', R5' :: Token
  -- Instructions

  -- | The @mv@ instruction
  Mv :: Token
  -- | The @ret@ instruction
  Ret :: Token
  -- | The @jmp@ instruction
  Jmp :: Token
  -- | The @call@ instruction
  Call :: Token
  -- | The @nop@ instruction
  Nop :: Token
  -- | The @salloc@ instruction
  Salloc :: Token
  -- | The @sfree@ instruction
  Sfree :: Token
  -- | The @sld@ instruction
  Sld :: Token
  -- | The @sst@ instruction
  Sst :: Token
  -- | The @ld@ instruction
  Ld :: Token
  -- | The @st@ instruction
  St :: Token
  -- | The @sref@ instruction
  Sref :: Token
  -- | The @and@ instruction
  And :: Token
  -- TODO: add more instructions
  -- Symbols

  -- | Opening symbols @(@, @[@, @{@ and @\<@
  LParen, LBrace, LBracket, LAngle :: Token
  -- | Closing symbols @)@, @]@, @}@ and @\>@
  RParen, RBrace, RBracket, RAngle :: Token
  -- | Pointer quantifier "@*@"
  Star :: Token
  -- | Literal quantifier "@$@"
  Dollar :: Token
  -- | Register quantifier "@%@"
  Percent :: Token
  -- | Separator "@,@"
  Comma :: Token
  -- | Separator "@:@"
  Colon :: Token
  -- | Separator "@::@"
  DoubleColon :: Token
  -- | Separator "@.@"
  Dot :: Token
  -- | Negation "@-@"
  Minus :: Token
  -- | Addition "@+@"
  Plus :: Token
  -- | Separator "@|@"
  Pipe :: Token
  -- | Separator "@=@"
  Equal :: Token
  -- | Separator "@->@" or "@→@"
  Arrow :: Token
  -- | Instruction separator "@;@"
  Semi :: Token
  -- | Special bang type
  Bang :: Token
  -- Keywords

  -- | \"@forall@\" (or "@∀@") type variable binder in type
  Forall :: Token
  -- | \"@unsafe@\" block
  UnSafe :: Token
  -- | \"@section@\" block
  Section :: Token
  -- \ \"@include@\" block
  Include :: Token
  -- | The @Ta@ kind
  TaK :: Token
  -- | The @Ts@ kind
  TsK :: Token
  -- | The @Tc@ kind
  TcK :: Token
  -- | The @T<n>@ kind
  TnK :: Integer -> Token
  -- Comments

  -- | A comment starting with "@#@" and spanning until the end of the current line
  InlineComment ::
    -- | The content of the comment
    Text ->
    Token
  -- | A comment starting with "@/\*@" and ending with "@\*/@"
  MultilineComment ::
    -- | The content of the comment
    Text ->
    Token
  -- | End Of Line
  EOL :: Token
  -- | End Of File
  EOF :: Token
  -- | An horizontal space
  HSpace :: Token

deriving instance Show Token

deriving instance Eq Token

deriving instance Ord Token

deriving instance Data Token

deriving instance Typeable Token

-- | A token with some location information attached
type LToken = Located Token
