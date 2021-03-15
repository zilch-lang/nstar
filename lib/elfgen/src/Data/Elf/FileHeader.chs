{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

{-# OPTIONS -Wno-duplicate-exports #-}

module Data.Elf.FileHeader
( ElfHeader(..)
, Class(..)
, Encoding(..)
, OSABI(..)
, ObjFileType(..)
, Arch(..)
, Version(..)
, module Data.Elf.FileHeader.Flags
  -- * C bits
, C_ElfFileHeader
, peekFileHeader, newFileHeader, freeFileHeader
) where

import Data.Elf.Types
import Data.Elf.FileHeader.Flags
import Data.Elf.Internal.BusSize (Size)
import Data.Word (Word8, Word32)
import Foreign.Storable (Storable(..))
import Foreign.Ptr (Ptr)
import Foreign.C.Types (CInt)
import Foreign.Marshal.Alloc (malloc, free)
import GHC.Generics (Generic)

#include "file_header.h"

data ElfHeader (n :: Size)
  = ElfHeader
      !Class         -- ^ ELF class (32-bit or 64-bit object)
      !Encoding      -- ^ Data encoding (little endian or big endian)
      !OSABI         -- ^ OS ABI identification
      !Word8         -- ^ ABI version
      !ObjFileType   -- ^ Object file type
      !Arch          -- ^ Architecture
      !Version       -- ^ Object file version (@'ev_current'@ or @'ev_none'@)
      !(EFlags n)    -- ^ Processor-specific flags
  deriving (Generic)

-- | ELF class
{#enum elf_class as Class {C_NONE as C_None, C_32 as C_32, C_64 as C_64} deriving (Generic)#}
-- | Data encoding
{#enum elf_encoding as Encoding {D_NONE as D_None, D_2LSB as D_2LSB, D_2MSB as D_2MSB} deriving (Generic)#}
-- | OS ABI identification
{#enum elf_osabi as OSABI {OSABI_NONE as OSABI_None, OSABI_SYSV as OSABI_SysV} deriving (Generic)#}
-- | Legal values for @'e_type'@
{#enum elf_file_type as ObjFileType {OFT_NONE as ET_None, OFT_REL as ET_Rel, OFT_EXEC as ET_Exec, OFT_DYN as ET_Dyn, OFT_CORE as ET_Core} deriving (Generic)#}
-- | Legal values for @'e_machine'@
{#enum elf_arch as Arch {MA_NONE as EM_None, MA_SPARC as EM_sparc, MA_X86_64 as EM_x86_64, MA_ARM as EM_arm} deriving (Generic)#}
-- | File version
{#enum elf_version as Version {VER_NONE as EV_None, VER_CURRENT as EV_Current} deriving (Generic)#}

---------------------------------------------------------------------------------------------------------------

data C_ElfFileHeader (n :: Size)
  = C_ElfFileHeader
      !CInt
      !CInt
      !CInt
      !Word8
      !CInt
      !CInt
      !CInt
      !Word32

instance Storable (C_ElfFileHeader n) where
  sizeOf _ = {#sizeof elf_file_header#}
  alignment _ = {#alignof elf_file_header#}
  peek ptr =
    C_ElfFileHeader <$> (fromIntegral <$> {#get struct elf_file_header->class#} ptr)
                    <*> (fromIntegral <$> {#get struct elf_file_header->encoding#} ptr)
                    <*> (fromIntegral <$> {#get struct elf_file_header->osabi#} ptr)
                    <*> (fromIntegral <$> {#get struct elf_file_header->osabi_version#} ptr)
                    <*> (fromIntegral <$> {#get struct elf_file_header->object_file_type#} ptr)
                    <*> (fromIntegral <$> {#get struct elf_file_header->arch#} ptr)
                    <*> (fromIntegral <$> {#get struct elf_file_header->version#} ptr)
                    <*> (fromIntegral <$> {#get struct elf_file_header->flags#} ptr)
  poke ptr (C_ElfFileHeader clas enc abi abiver typ arch ver flags) = do
    {#set struct elf_file_header->class#} ptr (fromIntegral clas)
    {#set struct elf_file_header->encoding#} ptr (fromIntegral enc)
    {#set struct elf_file_header->osabi#} ptr (fromIntegral abi)
    {#set struct elf_file_header->osabi_version#} ptr (fromIntegral abiver)
    {#set struct elf_file_header->object_file_type#} ptr (fromIntegral typ)
    {#set struct elf_file_header->arch#} ptr (fromIntegral arch)
    {#set struct elf_file_header->version#} ptr (fromIntegral ver)
    {#set struct elf_file_header->flags#} ptr (fromIntegral flags)

peekFileHeader :: Ptr (C_ElfFileHeader n) -> IO (ElfHeader n)
peekFileHeader ptr = do
  C_ElfFileHeader clas enc abi abiver typ arch ver flags <- peek ptr
  let elfClass = toEnum $ fromIntegral clas
      elfEncoding = toEnum $ fromIntegral enc
      elfABI = toEnum $ fromIntegral abi
      elfType = toEnum $ fromIntegral typ
      elfArch = toEnum $ fromIntegral arch
      elfVersion = toEnum $ fromIntegral ver
  pure (ElfHeader elfClass elfEncoding elfABI abiver elfType elfArch elfVersion (fromIntegral flags))

newFileHeader :: forall (n :: Size). ElfHeader n -> IO (Ptr (C_ElfFileHeader n))
newFileHeader (ElfHeader cls enc abi abiver typ arch ver flags) = do
  ptr <- malloc @(C_ElfFileHeader n)
  poke ptr $ C_ElfFileHeader (fromIntegral $ fromEnum cls) (fromIntegral $ fromEnum enc) (fromIntegral $ fromEnum abi) (fromIntegral abiver)
                             (fromIntegral $ fromEnum typ) (fromIntegral $ fromEnum arch) (fromIntegral $ fromEnum ver) (fromIntegral flags)
  pure ptr

freeFileHeader :: Ptr (C_ElfFileHeader n) -> IO ()
freeFileHeader ptr = do
  free ptr
