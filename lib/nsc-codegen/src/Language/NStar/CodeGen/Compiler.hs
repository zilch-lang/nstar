module Language.NStar.CodeGen.Compiler where

import Control.Monad.Writer (Writer)
import Data.Word (Word8)
import Data.Map (Map)
import Data.Text (Text)

type Compiler a = Writer MachineInfo a

-- | Machine information
data MachineInfo
  = MInfo
      [Word8]                -- ^ Generated executable machine code
      [(Text, SymbolType')]  -- ^ Symbol table associating function symbols with their types in the code
      DataTable              -- ^ Data table registering all available data labels and values

instance Semigroup MachineInfo where
  MInfo code1 syms1 d1 <> MInfo code2 syms2 d2 = MInfo (code1 <> code2) (syms1 <> syms2) (d1 <> d2)

instance Monoid MachineInfo where
  mempty = MInfo mempty mempty mempty

data SymbolType'
  = Function Integer
  | Object

data DataTable
  = DataTable [Text] [Word8]

instance Semigroup DataTable where
  DataTable l1 v1 <> DataTable l2 v2 = DataTable (l1 <> l2) (v1 <> v2)

instance Monoid DataTable where
  mempty = DataTable mempty mempty
