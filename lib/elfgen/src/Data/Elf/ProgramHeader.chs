{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Elf.ProgramHeader
( ProgramHeader(..)
, section, binaryData
, module Data.Elf.ProgramHeader.Flags
  -- * C bits
, C_ElfProgramHeader
, peekProgramHeader, newProgramHeader, freeProgramHeader
) where

import Data.Elf.Types
import Data.Elf.ProgramHeader.Flags
import Data.Elf.Internal.BusSize (Size)
import Data.Word (Word8, Word32)
import Foreign.Storable (Storable(..))
import Foreign.Ptr (Ptr, castPtr)
import Foreign.C.Types (CChar)
import Foreign.C.String (CString, peekCString, newCString)
import Foreign.Marshal.Array (peekArray, newArray)
import Foreign.Marshal.Alloc (malloc, free)
import GHC.Generics (Generic)

#include "segment_header.h"

-- | Program segment header
data ProgramHeader (n :: Size)
  -- | Entry for header table itself
  = PPhdr
  -- | Program header table entry unused
  | PNull
  -- | Loadable program segment
  | PLoad
      SectionOrData     -- ^ Either the name of a section or concrete binary data.
      (PFlags n)
  -- | Program interpreter
  | PInterp
      String     -- ^ Path to the dynamic interpreter
      (PFlags n)
  deriving (Generic)
deriving instance Eq (ProgramHeader n)
deriving instance Ord (ProgramHeader n)

-- | Either a section name, or concrete binary data.
type SectionOrData = Either String [Word8]

-- | Indicates that we want to fetch data from a specific section. This essentially is an alias for 'Left', to be used with data constructors from 'ProgramHeader'
--   in order to prevent duplicated binary data in the resulting ELF file (note that this duplication causes all sorts of problems like non-allocation of progbits).
section :: String -> SectionOrData
section = Left

-- | Indicates that we want to have direct binary data which is not present in any other section. This is an alias for 'Right'.
binaryData :: [Word8] -> SectionOrData
binaryData = Right

----------------------------------------------------------------------------------------------------------

-- | Segment header type
{#enum program_header_type as C_SegmentType {P_PHDR as CrPhdr, P_NULL as CrNull, P_LOAD as CrLoad, P_INTERP as CrInterp}#}
-- | Load segment type of encapsulated data
{#enum p_load_data_type as C_LoadType {P_LOAD_SECTION as LoadSection, P_LOAD_DATA as LoadData}#}

data C_ElfProgramHeader (n :: Size)
  = P_Phdr
  | P_Null
  | P_Load (Ptr ()) C_LoadType Word32 Word32
  | P_Interp CString Word32

instance Storable (C_ElfProgramHeader n) where
  sizeOf _ = {#sizeof elf_segment_header#}
  alignment _ = {#alignof elf_segment_header#}
  peek ptr =
    (toEnum . fromIntegral <$> {#get struct elf_segment_header->type#} ptr) >>= \ case
      CrPhdr   -> pure P_Phdr
      CrNull   -> pure P_Null
      CrLoad   -> P_Load <$> (castPtr <$> {#get struct elf_segment_header->data.p_load.data#} ptr)
                         <*> (toEnum . fromIntegral <$> {#get struct elf_segment_header->data.p_load.data_type#} ptr)
                         <*> (fromIntegral <$> {#get struct elf_segment_header->data.p_load.data_len#} ptr)
                         <*> (fromIntegral <$> {#get struct elf_segment_header->data.p_load.flags#} ptr)
      CrInterp -> P_Interp <$> {#get struct elf_segment_header->data.p_interp.interpreter_path#} ptr
                           <*> (fromIntegral <$> {#get struct elf_segment_header->data.p_interp.flags#} ptr)
  poke ptr P_Phdr = do
    {#set struct elf_segment_header->type#} ptr (fromIntegral $ fromEnum CrPhdr)
  poke ptr P_Null = do
    {#set struct elf_segment_header->type#} ptr (fromIntegral $ fromEnum CrNull)
  poke ptr (P_Interp ip flags) = do
    {#set struct elf_segment_header->type#} ptr (fromIntegral $ fromEnum CrInterp)
    {#set struct elf_segment_header->data.p_interp.interpreter_path#} ptr ip
    {#set struct elf_segment_header->data.p_interp.flags#} ptr (fromIntegral flags)
  poke ptr (P_Load d lt dl flags) = do
    {#set struct elf_segment_header->type#} ptr (fromIntegral $ fromEnum CrLoad)
    {#set struct elf_segment_header->data.p_load.data#} ptr (castPtr d)
    {#set struct elf_segment_header->data.p_load.data_type#} ptr (fromIntegral $ fromEnum lt)
    {#set struct elf_segment_header->data.p_load.data_len#} ptr (fromIntegral dl)
    {#set struct elf_segment_header->data.p_load.flags#} ptr (fromIntegral flags)

peekProgramHeader :: Ptr (C_ElfProgramHeader n) -> IO (ProgramHeader n)
peekProgramHeader ptr = do
  peek ptr >>= \ case
    P_Phdr               -> pure PPhdr
    P_Null               -> pure PNull
    P_Interp ip flags    -> PInterp <$> peekCString ip
                                 <*> pure (fromIntegral flags)
    P_Load d lt dl flags -> case lt of
      LoadSection -> PLoad <$> (Left <$> peekCString (castPtr d))
                           <*> pure (fromIntegral flags)
      LoadData    -> PLoad <$> (Right <$> peekArray (fromIntegral dl) (castPtr d))
                           <*> pure (fromIntegral flags)

newProgramHeader :: forall (n :: Size). ProgramHeader n -> IO (Ptr (C_ElfProgramHeader n))
newProgramHeader header = do
  ptr <- malloc @(C_ElfProgramHeader n)
  case header of
    PPhdr -> poke ptr P_Phdr
    PNull -> poke ptr P_Null
    PInterp ip flags -> do
      str <- newCString ip
      poke ptr $ P_Interp str (fromIntegral flags)
    PLoad (Left s) flags -> do
      str <- newCString s
      poke ptr $ P_Load (castPtr str) LoadSection 0 (fromIntegral flags)
    PLoad (Right d) flags -> do
      bytes <- newArray d
      poke ptr $ P_Load (castPtr bytes) LoadData (fromIntegral $ length d) (fromIntegral flags)
  pure ptr

freeProgramHeader :: Ptr (C_ElfProgramHeader n) -> IO ()
freeProgramHeader ptr = do
  peek ptr >>= \ case
    P_Phdr               -> pure ()
    P_Null               -> pure ()
    P_Interp ip flags    -> free ip
    P_Load d lt dl flags -> free d

  free ptr
