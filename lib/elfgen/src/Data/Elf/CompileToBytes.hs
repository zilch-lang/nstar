{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Data.Elf.CompileToBytes
( -- * Data kinds
  Size(..), Endianness(..)
  -- * Compilation
, compile
  -- * Re-exports
, module Data.ByteString.Lazy
) where

import Data.Elf.Internal.BusSize (Size(..))
import Data.Elf.Internal.Endianness (Endianness(..))
import Data.Elf.Internal.Serialize (Serializable(..), runPut)
import Data.Elf.Object (ElfObject)
import Data.ByteString.Lazy (ByteString, writeFile)
import Data.Elf.Internal.Compile (unabstract)
import Data.Elf.Internal.Object (Object)
import Data.Elf.Internal.FileHeader (Elf_Ehdr)
import Data.Elf.Internal.ProgramHeader (Elf_Phdr)
import Data.Elf.Internal.SectionHeader (Elf_Shdr)
import Data.Elf.Types (ValueSet)
import Data.Elf.Internal.Compile.ForArch (CompileFor)
import Data.Elf.FileHeader (ElfHeader)
import Data.Elf.ProgramHeader (ProgramHeader)
import Data.Elf.SectionHeader (SectionHeader)
import Data.Elf.Symbol (ElfSymbol)
import Data.Elf.Internal.Symbol (Elf_Sym)

-- | Completely compiles an abstract ELF file down to a 'ByteString' ready to be written to a file.
--
--   The ELF object undergoes two stages:
--
--   [An unabstracting stage] Where the abstract ELF file is broken down into a concrete representation ready to be serialized.
--   [A serialization stage] Where the concrete representation is compiled down to simple binary data following the ELF specifications.
compile :: forall (n :: Size) e.
           ( ValueSet n
           , Serializable n e (Elf_Ehdr n)
           , Serializable n e (Elf_Phdr n)
           , Serializable n e (Elf_Shdr n)
           , Serializable n e (Elf_Sym n)
           , CompileFor n ElfHeader Elf_Ehdr
           , CompileFor n ProgramHeader Elf_Phdr
           , CompileFor n SectionHeader Elf_Shdr
           , CompileFor n ElfSymbol Elf_Sym
           ) => Endianness e -> ElfObject n -> ByteString
compile e = runPut . put @n e . unabstract
