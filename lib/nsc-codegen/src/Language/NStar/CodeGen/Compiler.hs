module Language.NStar.CodeGen.Compiler where

import Control.Monad.Writer (Writer)
import Data.Word (Word8)
import Data.Text (Text)
import Data.Elf (RelocationType)

type Compiler a = Writer MachineInfo a

-- | Machine information
data MachineInfo
  = MInfo
      [Word8]                -- ^ Generated executable machine code
      [(Text, SymbolType')]  -- ^ Symbol table associating function symbols with their types in the code
      DataTable              -- ^ Data table registering all available data labels and values
      [RelocType']           -- ^ All relocation table entries

instance Semigroup MachineInfo where
  MInfo code1 syms1 d1 r1 <> MInfo code2 syms2 d2 r2 = MInfo (code1 <> code2) (syms1 <> syms2) (d1 <> d2) (r1 <> r2)

instance Monoid MachineInfo where
  mempty = MInfo mempty mempty mempty mempty

data SymbolType'
  = Function Integer Bool
  | Object Integer

data DataTable
  = DataTable [(Text, Integer)] [Word8]

instance Semigroup DataTable where
  DataTable l1 v1 <> DataTable l2 v2 = DataTable (l1 <> l2) (v1 <> v2)

instance Monoid DataTable where
  mempty = DataTable mempty mempty

data RelocType'
  -- | A relocation entry in the .text section
  = RelocText
      Text            -- ^ The symbol name
      Text            -- ^ The section the symbol originates from
      Integer         -- ^ The offset from the symbol
      Integer         -- ^ The offset where to relocate in the @.text@ section
      RelocationType  -- ^ The relocation type
