{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE KindSignatures #-}

module Language.NStar.CodeGen.Elf.Builder (compileToElf) where

import Data.Elf hiding (compile)
import Language.NStar.CodeGen.Machine
import Data.Bits ((.|.))
import Language.NStar.CodeGen.Arch (SupportedArch(..))
import Language.NStar.Typechecker.Core (TypedProgram)
import Language.NStar.CodeGen.Compiler (MachineInfo(..), SymbolType'(..), DataTable(..))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text (unpack)

class CompileToElf (n :: Size) where
  compileToElf :: SupportedArch -> TypedProgram -> ElfObject n

instance CompileToElf S64 where
  compileToElf arch prog =
    let MInfo opcodes syms (DataTable dataLabels dataSect) = compile arch prog
    in ElfObject
        (ElfHeader (supportedArchToClass arch) (supportedArchToEncoding arch) OSABI_None 0x0 ET_Rel (supportedArchToArch arch) EV_Current ef_none)
        [ PLoad (section ".text") (pf_r .|. pf_x)
        , PLoad (section ".data") (pf_r .|. pf_w) ]
        [ SProgBits ".text" opcodes (shf_alloc .|. shf_execinstr)
        , SProgBits ".data" dataSect (shf_alloc .|. shf_write)
--        , SRela ".rela.text" textReloc
        , SSymTab ".symtab" (generateSymbolTableFrom @S64 syms <> generateDataSymbols @S64 dataLabels) ]


generateSymbolTableFrom :: forall (n :: Size). [(Text, SymbolType')] -> [ElfSymbol n]
generateSymbolTableFrom = fmap \ (k, l) ->
  let symType = case l of
        Function idx -> ST_Func idx
        Object       -> ST_Object
  in ElfSymbol (Text.unpack k) symType SB_Global SV_Default

generateDataSymbols :: [Text] -> [ElfSymbol n]
generateDataSymbols = fmap \ n -> ElfSymbol (Text.unpack n) ST_Object SB_Local SV_Default

supportedArchToArch :: SupportedArch -> Arch
supportedArchToArch X64 = EM_x86_64

supportedArchToClass :: SupportedArch -> Class
supportedArchToClass X64 = C_64

supportedArchToEncoding :: SupportedArch -> Encoding
supportedArchToEncoding X64 = D_2LSB
