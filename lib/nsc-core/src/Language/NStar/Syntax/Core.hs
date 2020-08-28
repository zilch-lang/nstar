{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}

{-|
  Module: Language.NStar.Syntax.Core
  Description: NStar's core language
  Copyright: (c) Mesabloo, 2020
  License: BSD3
  Stability: experimental

  This module contains all the definitions of all AST nodes and Tokens that will be
  used across the whole compiler.
-}

module Language.NStar.Syntax.Core
where

import Data.Located
import Data.Text (Text)
import Data.Map (Map)
import Numeric.Natural (Natural)
import Data.Some (Some)

newtype Program
  = Program [Located Statement]  -- ^ A program is a possibily empty list of statements

-- | A statement is either
data Statement where
  -- | A typed label
  Label :: Located Text           -- ^ The label's name. It may not be empty
        -> Located (Type 'N)      -- ^ The "label's type", describing the minimal type expected when jumping to this label
        -> Statement
  -- | An instruction call
  Instr :: Instruction -> Statement

data Type :: RType -> * where
  -- | Signed integer
  Signed :: Natural                                        -- ^ The size of the integer (a multiple of 2 greater than 4)
         -> Type 'N
  -- | Unsigned integer
  Unsigned :: Natural                                      -- ^ The size of the integer (a multiple of 2 greater than 4)
           -> Type 'N
  -- | Stack constructor
  Cons :: Located (Type t1)                                -- ^ Stack head
       -> Located (Type 'S)                                -- ^ Stack tail
       -> Type 'S
  -- | Type variable
  Var :: Located Text                                      -- ^ The name of the type variable
      -> Type t
  -- | Record type
  Record :: Map (Located (Register r)) (Located (Type t))  -- ^ A mapping from 'Register's to their expected 'Type's
         -> Type 'N
  -- | Pointer to a normal type
  Ptr :: Located (Type 'N)
      -> Type 'N
  -- | Pointer to a stack type
  --
  -- We separate stack pointers from normal pointers
  -- because they are used as a different construct
  -- in the source code (@*ty@ vs @sptr sty@)
  SPtr :: Located (Type 'S)
       -> Type 'S

-- | Register type
data RType
  = S -- ^ The register is a stack. It can be indexed, @push@ed and @pop@ped
  | N -- ^ the register is normal. It only permits reading and writing

data Register :: RType -> * where
  -- | 64 bits single-value registers
  RAX, RBX, RCX, RDX, RDI, RSI, RBP :: Register 'N
  -- | 64 bits stack registers
  RSP :: Register 'S
  {- We do not permit using registers like %rip, %flags, etc.
     Those are used internally by some instructions.
  -}

-- | N*'s instruction set
data Instruction where
  -- | @mov a, v@ is the same as @a <- v@
  MOV :: Located (Expr 'A)    -- ^ The destination of the move. It must be addressable
      -> Located (Expr 'V)    -- ^ The source value moved into the destination
      -> Instruction

  -- TODO: add more instructions

-- | Expression type
data EType
  = V -- ^ The expression is a value. It can neither be offset, indexed, nor set
  | A -- ^ The expression is addressable. It can be offset, indexed, and set

data Expr :: EType -> * where
  -- | An immediate value (@$⟨val⟩@)
  Imm :: Located (Immediate i)      -- ^ \- @⟨val⟩@
      -> Expr 'V
  -- | A label name
  Name :: Located Text
       -> Expr e
  -- | An address (@⟨addr⟩@)
  Address :: Located Integer        -- ^ \- @⟨addr⟩@
          -> Expr e
  -- | An indexed expression (@⟨idx⟩[⟨expr⟩]@)
  Indexed :: Located Integer        -- ^ \- @⟨idx⟩@
          -> Located (Expr 'A)      -- ^ \- @⟨expr⟩@
          -> Expr e
  -- | A register (one of the available 'Register's)
  Reg :: Located (Register r)
      -> Expr e
  -- | Type specialization (used on a @call@) (@⟨expr⟩\<⟨type⟩\>@)
  Spec :: Located (Expr e)          -- ^ \- @⟨expr⟩@
       -> Located (Type t)          -- ^ \- @⟨type⟩@
       -> Expr e

data Immediate v where
  -- | An integer, either in decimal, hexadecimal, octal or binary format
  --
  -- Grammars are:
  --
  -- * Binary: @0(b|B)(0|1)⁺@
  -- * Octal: @0(o|O)(0..7)⁺@
  -- * Decimal: @(0..9)⁺@
  -- * Hexadecimal: @0(x|X)(0..9|A..F|a..f)⁺@
  I :: Integer -> Immediate Integer
  -- | A character (which can be an escape sequence)
  C :: Char -> Immediate Char


------------------------------------------------------------------------------------------------

data Token v where
  -- | Any literal value like integers or characters
  Literal :: l             -- ^ The value of the literal
          -> Token l
  -- | An identifier (also called name)
  Id :: Text -> Token Text
  -- Registers
  -- | Registers reserved words
  Rax, Rbx, Rcx, Rdx, Rdi, Rsi, Rsp, Rbp :: Token ()
  -- Instructions
  -- | The @mov@ instruction
  Mov :: Token ()
  -- TODO: add more instructions
  -- Symbols
  -- | Opening symbols @(@, @[@, @{@ and @\<@
  LParen, LBrace, LBracket, LAngle :: Token ()
  -- | Closing symbols @)@, @]@, @}@ and @\>@
  RParen, RBrace, RBracket, RAngle :: Token ()
  -- | Pointer quantifier "@*@"
  Star :: Token ()
  -- | Literal quantifier "@$@"
  Dollar :: Token ()
  -- | Register quantifier "@%@"
  Percent :: Token ()
  -- | Separator "@,@"
  Comma :: Token ()
  -- | Separator "@:@"
  Colon :: Token ()
  -- | Separator "@.@"
  Dot :: Token ()
  -- Keywords
  -- | \"@forall@\" type variable binder in type
  Forall :: Token ()
  -- | \"@sptr@\" stack pointer quantifier
  Sptr :: Token ()
  -- Comments
  -- | A comment starting with "@#@" and spanning until the end of the current line
  InlineComment :: Text        -- ^ The content of the comment
                -> Token ()
  -- | A comment starting with "@/\*@" and ending with "@\*/@"
  MultilineComment :: Text     -- ^ The content of the comment
                   -> Token ()
  -- | End Of Line
  EOL :: Token ()
  -- | End Of File
  EOF :: Token ()

type LToken l = Located (Token l)

type SomeLToken = Located (Some Token)
