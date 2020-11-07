module Language.NStar.CodeGen.Compiler where

import Control.Monad.Writer (Writer)
import Data.Word (Word8)
import Data.Map (Map)
import Data.Text (Text)

type Compiler a = Writer MachineInfo a

-- | Machine information
data MachineInfo
  = MInfo
      [Word8]             -- ^ Generated executable machine code
      (Map Text Integer)  -- ^ Symbol table associating symbols with their offsets in the code

instance Semigroup MachineInfo where
  MInfo code1 syms1 <> MInfo code2 syms2 = MInfo (code1 <> code2) (syms1 <> syms2)

instance Monoid MachineInfo where
  mempty = MInfo mempty mempty
