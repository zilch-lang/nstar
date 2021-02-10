{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

{-|
  Module: Language.NStar.Syntax.Core
  Description: NStar's syntactic core language
  Copyright: (c) Mesabloo, 2020
  License: BSD3
  Stability: experimental

  This module contains all the definitions of all AST nodes and Tokens that will be
  used across the whole compiler.

  The translation from the source code to the syntactic core should be almost a 1:1 conversion,
  so a program can be printed back as it was. Both lexing and parsing steps must be reversible steps.
-}

module Language.NStar.Syntax.Core
where

import Data.Located
import Data.Text (Text)
import Data.Map (Map)
import Numeric.Natural (Natural)
import Data.Data (Data)
import Data.Typeable (Typeable)

newtype Program
  = Program [Located Section]  -- ^ A program is a (possibly empty) list of sections

deriving instance Show Program

data Section where
  -- | The @code@ section
  Code :: [Located Statement]
       -> Section
  -- | The @data@ section
  Data :: [Located Binding]
       -> Section
  -- | The @rodata@ section
  ROData :: [Located Binding]
         -> Section
  -- | The @udata@ section
  UData :: [Located ReservedSpace]
        -> Section

deriving instance Show Section

data Binding where
  -- | An initialized data binding
  Bind :: Located Text
       -> Located Type
       -> Located Constant
       -> Binding

deriving instance Show Binding

data ReservedSpace where
  -- | An uninitialized data binding (in the @udata@ section)
  ReservedBind :: Located Text
               -> Located Type
               -> ReservedSpace

deriving instance Show ReservedSpace

-- | A statement is either
data Statement where
  -- | A typed label
  Label :: Located Text           -- ^ The label's name. It may not be empty
        -> Located Type           -- ^ The "label's type", describing the minimal type expected when jumping to this label
        -> [Located Instruction]  -- ^ Instructions contained in its block
        -> Statement
  -- | An unsafe block
  Unsafe :: [Located Statement] -> Statement

deriving instance Show Statement

data Type where
  -- | Signed integer
  Signed :: Natural                                        -- ^ The size of the integer (a multiple of 2 greater than 4)
         -> Type
  -- | Unsigned integer
  Unsigned :: Natural                                      -- ^ The size of the integer (a multiple of 2 greater than 4)
           -> Type
  -- | Stack constructor
  Cons :: Located Type                                     -- ^ Stack head
       -> Located Type                                     -- ^ Stack tail
       -> Type
  -- | Type variable
  Var :: Located Text                                      -- ^ The name of the type variable
      -> Type
  -- | Free type variable
  FVar :: Located Text                                     -- ^ The name of the type variable
       -> Type
  -- | Record type
  Record :: Map (Located Register) (Located Type)          -- ^ A mapping from 'Register's to their expected 'Type's
         -> Located Type                                   -- ^ The stack required on this context
         -> Located Type                                   -- ^ The return continuation
         -> Bool                                           -- ^ Is the record opened or closed?
         -> Type
  -- | Pointer to a normal type
  Ptr :: Located Type
      -> Type
  -- | Pointer to a stack type
  --
  -- We separate stack pointers from normal pointers
  -- because they are used as a different construct
  -- in the source code (@*ty@ vs @sptr sty@)
  SPtr :: Located Type
       -> Type
  -- | Forall type variable binder
  ForAll :: [(Located Type, Located Kind)]                  -- ^ Variables along with their 'Kind's
         -> Located Type
         -> Type
  -- | Register type
  Register :: Natural                                       -- ^ Register size
           -> Type

deriving instance Show Type
deriving instance Eq Type

data Kind where
  -- | Kind of 8-bytes big types
  T8 :: Kind
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
  -- | Pointer register
  SP, BP :: Register

deriving instance Show Register
deriving instance Eq Register
deriving instance Ord Register

-- | N*'s instruction set
data Instruction where
  -- | @mov a, v@ is the same as @v <- a@.
  MOV :: Located Expr         -- ^ The destination of the move. It must be addressable
      -> Located Expr         -- ^ The source value moved into the destination
      -> Instruction
  -- | @ret@ returns to the address on top of the stack.
  RET :: Instruction
  -- | @jmp@ alters the control flow by unconditionally jumping to the given address.
  JMP :: Located Expr
      -> [Located Type]
      -> Instruction
  -- | @call@ alters the control flow by pushing the current address onto the stack and jumping
  --   to the given address (either as a label or in a register).
  CALL :: Located Expr
       -> [Located Type]
       -> Instruction
  -- |
  ADD :: Located Expr    -- ^ The source operand
      -> Located Expr    -- ^ The increment value
      -> Instruction
  -- |
  PUSH :: Located Expr
       -> Instruction
  -- |
  POP :: Located Expr
      -> Instruction
  -- |
  SUB :: Located Expr
      -> Located Expr
      -> Instruction
  -- |
  NOP :: Instruction

  -- TODO: add more instructions

deriving instance Show Instruction

data Constant where
  -- | A constant integer
  CInteger :: Located Integer
           -> Constant
  -- | A constant character
  CCharacter :: Located Char
             -> Constant
  -- | An array of constants
  CArray :: [Located Constant]
         -> Constant

deriving instance Show Constant

data Expr where
  -- | An immediate value (@$⟨val⟩@)
  Imm :: Located Immediate          -- ^ \- @⟨val⟩@
      -> Expr
  -- | A label name
  Name :: Located Text
       -> Expr
  -- | An indexed expression (@⟨idx⟩[⟨expr⟩]@)
  Indexed :: Located Expr           -- ^ \- @⟨idx⟩@
          -> Located Expr           -- ^ \- @⟨expr⟩@
          -> Expr
  -- | A register (one of the available 'Register's)
  Reg :: Located Register
      -> Expr
  -- | Type specialization (used on a @call@) (@⟨expr⟩\<⟨type⟩\>@)
  Spec :: Located Expr             -- ^ \- @⟨expr⟩@
       -> Located Type             -- ^ \- @⟨type⟩@
       -> Expr

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
  -- Registers
  -- | Registers reserved words
  R0', R1', R2', R3', R4', R5', SP', BP' :: Token
  -- Instructions
  -- | The @mov@ instruction
  Mov :: Token
  -- | The @ret@ instruction
  Ret :: Token
  -- | The @jmp@ instruction
  Jmp :: Token
  -- | The @call@ instruction
  Call :: Token
  -- | The @push@ instruction
  Push :: Token
  -- | The @pop@ instruction
  Pop :: Token
  -- | The @nop@ instruction
  Nop :: Token
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
  -- Keywords
  -- | \"@forall@\" (or "@∀@") type variable binder in type
  Forall :: Token
  -- | \"@sptr@\" stack pointer quantifier
  Sptr :: Token
  -- | \"@unsafe@\" block
  UnSafe :: Token
  -- | \"@section@\" block
  Section :: Token
  -- Comments
  -- | A comment starting with "@#@" and spanning until the end of the current line
  InlineComment :: Text        -- ^ The content of the comment
                -> Token
  -- | A comment starting with "@/\*@" and ending with "@\*/@"
  MultilineComment :: Text     -- ^ The content of the comment
                   -> Token
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
