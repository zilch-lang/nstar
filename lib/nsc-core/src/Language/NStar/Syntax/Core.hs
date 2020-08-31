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
  = Program [Located Statement]  -- ^ A program is a possibily empty list of statements

-- | A statement is either
data Statement where
  -- | A typed label
  Label :: Located Text           -- ^ The label's name. It may not be empty
        -> Located Type           -- ^ The "label's type", describing the minimal type expected when jumping to this label
        -> Statement
  -- | An instruction call
  Instr :: Instruction -> Statement

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
  -- | Record type
  Record :: Map (Located Register) (Located Type)          -- ^ A mapping from 'Register's to their expected 'Type's
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

data Kind where
  -- | Kind of 8-bytes big types
  T8 :: Kind
  -- | Kind of stack types
  Ts :: Kind
  -- | Kind of unsized types
  Ta :: Kind

data Register where
  -- | 64 bits single-value registers
  RAX, RBX, RCX, RDX, RDI, RSI, RBP :: Register
  -- | 64 bits stack registers
  RSP :: Register
  {- We do not permit using registers like %rip, %flags, etc.
     Those are used internally by some instructions.
  -}

-- | N*'s instruction set
data Instruction where
  -- | @mov a, v@ is the same as @a <- v@.
  MOV :: Located Expr         -- ^ The destination of the move. It must be addressable
      -> Located Expr         -- ^ The source value moved into the destination
      -> Instruction
  -- | @ret@ returns the value in @'RAX'@ to the caller.
  RET :: Instruction

  -- TODO: add more instructions

data Expr where
  -- | An immediate value (@$⟨val⟩@)
  Imm :: Located Immediate          -- ^ \- @⟨val⟩@
      -> Expr
  -- | A label name
  Name :: Located Text
       -> Expr
  -- | An indexed expression (@⟨idx⟩[⟨expr⟩]@)
  Indexed :: Located Integer        -- ^ \- @⟨idx⟩@
          -> Located Expr           -- ^ \- @⟨expr⟩@
          -> Expr
  -- | A register (one of the available 'Register's)
  Reg :: Located Register
      -> Expr
  -- | Type specialization (used on a @call@) (@⟨expr⟩\<⟨type⟩\>@)
  Spec :: Located Expr             -- ^ \- @⟨expr⟩@
       -> Located Type             -- ^ \- @⟨type⟩@
       -> Expr

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


------------------------------------------------------------------------------------------------

data Token where
  -- Literals
  -- | A literal integer
  Integer :: Integer -> Token
  -- | A literal character
  Char :: Char -> Token
  -- | An identifier (also called name)
  Id :: Text -> Token
  -- Registers
  -- | Registers reserved words
  Rax, Rbx, Rcx, Rdx, Rdi, Rsi, Rsp, Rbp :: Token
  -- Instructions
  -- | The @mov@ instruction
  Mov :: Token
  -- | The @ret@ instruction
  Ret :: Token
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
  -- Keywords
  -- | \"@forall@\" type variable binder in type
  Forall :: Token
  -- | \"@sptr@\" stack pointer quantifier
  Sptr :: Token
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

deriving instance Show Token
deriving instance Eq Token
deriving instance Ord Token
deriving instance Data Token
deriving instance Typeable Token

-- | A token with some location information attached
type LToken = Located Token
