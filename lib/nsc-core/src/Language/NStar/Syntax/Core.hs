{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}

module Language.NStar.Syntax.Core
where

import Data.Located
import Data.Text (Text)
import Data.Map (Map)
import Numeric.Natural (Natural)


newtype Program
  = Program [Located Statement]  -- ^ A program is a possibily empty list of statements

-- | A statement is either
data Statement where
  Label :: Located Text           -- ^ The label's name. It may not be empty
        -> Located (Type 'N)      -- ^ The "label's type", describing the minimal type expected when jumping to this label
        -> Statement              -- ^ A typed label
  Instr :: Instruction -> Statement                              -- ^ An instruction call

data Type :: RType -> * where
  Signed :: Natural                                        -- ^ The size of the integer (a multiple of 2 greater than 4)
         -> Type 'N                                        -- ^ Signed integer
  Unsigned :: Natural                                      -- ^ The size of the integer (a multiple of 2 greater than 4)
           -> Type 'N                                      -- ^ Unsigned integer
  Cons :: Located (Type t1)                                -- ^ Stack head
       -> Located (Type 'S)                                -- ^ Stack tail
       -> Type 'S                                          -- ^ Stack constructor
  Var :: Located Text                                      -- ^ The name of the type variable
      -> Type t                                            -- ^ Type variable
  Record :: Map (Located (Register r)) (Located (Type t))  -- ^ A mapping from 'Register's to their expected 'Type's
         -> Type 'N                                        -- ^ Record type
  Ptr :: Located (Type 'N)
      -> Type 'N                                           -- ^ Pointer to a normal type
  SPtr :: Located (Type 'S)
       -> Type 'S                                          -- ^ Pointer to a stack type
                                                           --
                                                           -- We separate stack pointers from normal pointers
                                                           -- because they are used as a different construct
                                                           -- in the source code (@*ty@ vs @sptr sty@)

-- | Register type
data RType
  = S -- ^ The register is a stack. It can be indexed, @push@ed and @pop@ped
  | N -- ^ the register is normal. It only permits reading and writing

data Register :: RType -> * where
  -- 64 bits registers
  RAX, RBX, RCX, RDX, RDI, RSI, RBP :: Register 'N
  RSP :: Register 'S
  {- We do not permit using registers like %rip, %flags, etc.
     Those are used internally by some instructions.
  -}

-- | N*'s instruction set
data Instruction where
  MOV :: Located (Expr 'A)    -- ^ The destination of the move. It must be addressable
      -> Located (Expr 'V)    -- ^ The source value moved into the destination
      -> Instruction          -- ^ @mov a, v@ is the same as @a <- v@

-- | Expression type
data EType
  = V -- ^ The expression is a value. It cannot neither be offset, indexed, nor set
  | A -- ^ The expression is addressable. It can be offset, indexed, and set

data Expr :: EType -> * where
  Imm :: Located (Immediate i)      -- ^ \- @⟨val⟩@
      -> Expr 'V                    -- ^ An immediate value (@$⟨val⟩@)
  Name :: Located Text
       -> Expr e                    -- ^ A label name
  Address :: Located Integer        -- ^ \- @⟨addr⟩@
          -> Expr e                 -- ^ An address (@⟨addr⟩@)
  Indexed :: Located Integer        -- ^ \- @⟨idx⟩@
          -> Located (Expr 'A)      -- ^ \- @⟨expr⟩@
          -> Expr e                 -- ^ An indexed expression (@⟨idx⟩[⟨expr⟩]@)
  Reg :: Located (Register r)
      -> Expr e                     -- ^ A register (one of the available 'Register's)
  Spec :: Located (Expr e)          -- ^ \- @⟨expr⟩@
       -> Located (Type t)          -- ^ \- @⟨type⟩@
       -> Expr e                    -- ^ Type specialization (used on a @call@) (@⟨expr⟩\<⟨type⟩\>@)

data Immediate v where
  I :: Integer -> Immediate Integer -- ^ An integer, either in decimal, hexadecimal, octal or binary format
                                    --
                                    -- Grammars are:
                                    --
                                    -- \- Binary: @0(b|B)(0|1)⁺@
                                    --
                                    -- \- Octal: @0(o|O)(0..7)⁺@
                                    --
                                    -- \- Decimal: @(0..9)⁺@
                                    --
                                    -- \- Hexadecimal: @0(x|X)(0..9|A..F|a..f)⁺@
  C :: Char -> Immediate Char       -- ^ A character (which can be an escape sequence)


------------------------------------------------------------------------------------------------

data Token v where
  Literal :: l -> Token l                             -- ^ Any literal value like integer or characters
  -- Registers
  Rax, Rbx, Rcx, Rdx, Rdi, Rsi, Rsp, Rbp :: Token ()  -- ^ Registers reserved words
  -- Instructions
  Mov :: Token ()                                     -- ^ The @mov@ instruction

  -- TODO: add more instructions
  -- Symbols
  LParen, LBrace, LBracket, LAngle :: Token ()        -- ^ Opening symbols @(@, @[@, @{@ and @\<@
  RParen, RBrace, RBracket, RAngle :: Token ()        -- ^ Closing symbols @)@, @]@, @}@ and @\>@
  Star :: Token ()                                    -- ^ Pointer quantifier "@*@"
  Dollar :: Token ()                                  -- ^ Literal quantifier "@$@"
  Percent :: Token ()                                 -- ^ Register quantifier "@%@"
  Comma :: Token ()                                   -- ^ Separator "@,@"
  Colon :: Token ()                                   -- ^ Separator "@:@"
  Dot :: Token ()                                     -- ^ Separator "@.@"
  -- Keywords
  Forall :: Token ()                                  -- ^ \"Forall\" type variable binder in type
  Sptr :: Token ()                                    -- ^ \"sptr\" stack pointer quantifier
  -- Comments
  InlineComment :: Text -> Token ()                   -- ^ A comment starting with "@#@" and spanning until the end of the current line
  MultilineComment :: Text -> Token ()                -- ^ A comment starting with "@/\*@" and ending with "@\*/@".

type LToken l = Located (Token l)
