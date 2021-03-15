{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Elf.SectionHeader
( SectionHeader(..)
, module Data.Elf.SectionHeader.Flags
  -- * C bits
, C_ElfSectionHeader
, peekSectionHeader, newSectionHeader, freeSectionHeader
) where

import Data.Elf.Types
import Data.Elf.Internal.BusSize (Size)
import Data.Word (Word8, Word32, Word64)
import Data.Elf.SectionHeader.Flags
import Data.Elf.Symbol
import Foreign.Storable (Storable(..))
import Foreign.C.Types (CULong, CInt)
import Foreign.C.String (CString, peekCStringLen, peekCString, newCString, newCStringLen)
import Foreign.Marshal.Array (newArray, peekArray)
import Foreign.Ptr (Ptr, castPtr)
import Data.List (intercalate)
import Foreign.Marshal.Alloc (malloc, free)
import Debug.Trace
import GHC.Generics (Generic)

#include "section_header.h"

-- | Section header
data SectionHeader (n :: Size)
  -- | Section header table entry unused
  = SNull
  -- | Program data
  | SProgBits
      String
      [Word8]
      (SFlags n)
  -- | Relocation entries with addends
  | SRela
      String
      [RelocationSymbol n]
  -- | Program space with no data (bss)
  | SNoBits
      String
      Integer   -- ^ Space size
      (SFlags n)
  -- | Symbol table
  | SSymTab
      String
      [ElfSymbol n]
  -- | String table
  | SStrTab
      String
      [String]
  deriving (Generic)
deriving instance Eq (SectionHeader n)
deriving instance Ord (SectionHeader n)

{#enum section_type as C_SectionType {S_NULL as CrNull, S_PROGBITS as CrProgBits, S_RELA as CrRela, S_NOBITS as CrNoBits, S_SYMTAB as CrSymTab, S_STRTAB as CrStrTab}#}

data C_ElfSectionHeader (n :: Size)
  = S_Null
  | S_ProgBits CString (Ptr Word8) Word64 CULong
  | S_Rela CString (Ptr (Ptr (C_RelocationSymbol n))) CULong
  | S_NoBits CString Word32 Word64
  | S_SymTab CString (Ptr (Ptr (C_ElfSymbol n))) CULong
  | S_StrTab CString CString CULong

instance Storable (C_ElfSectionHeader n) where
  sizeOf _ = {#sizeof elf_section_header#}
  alignment _ = {#alignof elf_section_header#}
  peek ptr = (toEnum . fromIntegral <$> {#get struct elf_section_header->type#} ptr) >>= \ case
    CrNull -> pure S_Null
    CrProgBits -> S_ProgBits <$> {#get struct elf_section_header->data.s_progbits.section_name#} ptr
                             <*> (castPtr <$> {#get struct elf_section_header->data.s_progbits.binary_data#} ptr)
                             <*> (fromIntegral <$> {#get struct elf_section_header->data.s_progbits.flags#} ptr)
                             <*> {#get struct elf_section_header->data.s_progbits.binary_data_len#} ptr
    CrRela     -> S_Rela <$> {#get struct elf_section_header->data.s_rela.section_name#} ptr
                         <*> (castPtr <$> {#get struct elf_section_header->data.s_rela.symbols#} ptr)
                         <*> {#get struct elf_section_header->data.s_rela.symbols_len#} ptr
    CrNoBits   -> S_NoBits <$> {#get struct elf_section_header->data.s_nobits.section_name#} ptr
                           <*> (fromIntegral <$> {#get struct elf_section_header->data.s_nobits.space#} ptr)
                           <*> (fromIntegral <$> {#get struct elf_section_header->data.s_nobits.flags#} ptr)
    CrSymTab   -> S_SymTab <$> {#get struct elf_section_header->data.s_symtab.section_name#} ptr
                           <*> (castPtr <$> {#get struct elf_section_header->data.s_symtab.symbols#} ptr)
                           <*> {#get struct elf_section_header->data.s_symtab.symbols_len#} ptr
    CrStrTab   -> S_StrTab <$> {#get struct elf_section_header->data.s_strtab.section_name#} ptr
                           <*> {#get struct elf_section_header->data.s_strtab.strings#} ptr
                           <*> {#get struct elf_section_header->data.s_strtab.strings_len#} ptr
  poke ptr S_Null = do
    {#set struct elf_section_header->type#} ptr (fromIntegral $ fromEnum CrNull)
  poke ptr (S_ProgBits n d flags dl) = do
    {#set struct elf_section_header->type#} ptr (fromIntegral $ fromEnum CrProgBits)
    {#set struct elf_section_header->data.s_progbits.section_name#} ptr n
    {#set struct elf_section_header->data.s_progbits.binary_data#} ptr (castPtr d)
    {#set struct elf_section_header->data.s_progbits.flags#} ptr (fromIntegral flags)
    {#set struct elf_section_header->data.s_progbits.binary_data_len#} ptr dl
  poke ptr (S_Rela n syms symsl) = do
    {#set struct elf_section_header->type#} ptr (fromIntegral $ fromEnum CrRela)
    {#set struct elf_section_header->data.s_rela.section_name#} ptr n
    {#set struct elf_section_header->data.s_rela.symbols#} ptr (castPtr syms)
    {#set struct elf_section_header->data.s_rela.symbols_len#} ptr symsl
  poke ptr (S_NoBits n space flags) = do
    {#set struct elf_section_header->type#} ptr (fromIntegral $ fromEnum CrNoBits)
    {#set struct elf_section_header->data.s_nobits.section_name#} ptr n
    {#set struct elf_section_header->data.s_nobits.space#} ptr (fromIntegral space)
    {#set struct elf_section_header->data.s_nobits.flags#} ptr (fromIntegral flags)
  poke ptr (S_SymTab n syms symsl) = do
    {#set struct elf_section_header->type#} ptr (fromIntegral $ fromEnum CrSymTab)
    {#set struct elf_section_header->data.s_symtab.section_name#} ptr n
    {#set struct elf_section_header->data.s_symtab.symbols#} ptr (castPtr syms)
    {#set struct elf_section_header->data.s_symtab.symbols_len#} ptr symsl
  poke ptr (S_StrTab n s sl) = do
    {#set struct elf_section_header->type#} ptr (fromIntegral $ fromEnum CrStrTab)
    {#set struct elf_section_header->data.s_strtab.section_name#} ptr n
    {#set struct elf_section_header->data.s_strtab.strings#} ptr s
    {#set struct elf_section_header->data.s_strtab.strings_len#} ptr sl

peekSectionHeader :: Ptr (C_ElfSectionHeader n) -> IO (SectionHeader n)
peekSectionHeader ptr =
  peek ptr >>= \ case
    S_Null                  -> pure SNull
    S_ProgBits n d flags dl ->
      SProgBits <$> peekCString n
                <*> peekArray (fromIntegral dl) d
                <*> pure (fromIntegral flags)
    S_Rela n syms symsl     ->
      SRela <$> peekCString n
            <*> (traverse peekRelocationSymbol =<< peekArray (fromIntegral symsl) syms)
    S_NoBits n space flags  ->
      SNoBits <$> peekCString n
              <*> pure (fromIntegral space)
              <*> pure (fromIntegral flags)
    S_SymTab n syms symsl   ->
      SSymTab <$> peekCString n
              <*> (traverse peekElfSymbol =<< peekArray (fromIntegral symsl) syms)
    S_StrTab n s sl         ->
      SStrTab <$> peekCString n
              <*> (split (== '\0') <$> peekCStringLen (s, fromIntegral sl))

newSectionHeader :: forall (n :: Size). SectionHeader n -> IO (Ptr (C_ElfSectionHeader n))
newSectionHeader sect = do
  ptr <- malloc @(C_ElfSectionHeader n)

  case sect of
    SNull -> poke ptr S_Null
    SProgBits n d flags -> do
      bytes <- newArray d
      str <- newCString n
      poke ptr $ S_ProgBits str bytes (fromIntegral flags) (fromIntegral $ length d)
    SRela n syms        -> do
      str <- newCString n
      symbols <- newArray =<< traverse newRelocationSymbol syms
      poke ptr $ S_Rela str symbols (fromIntegral $ length syms)
    SNoBits n s flags   -> do
      str <- newCString n
      poke ptr $ S_NoBits str (fromIntegral s) (fromIntegral flags)
    SSymTab n syms      -> do
      str <- newCString n
      symbols <- newArray =<< traverse newElfSymbol syms
      poke ptr $ S_SymTab str symbols (fromIntegral $ length syms)
    SStrTab n strs      -> do
      str <- newCString n
      (strings, len) <- newCStringLen $ '\0' : intercalate "\0" strs
      poke ptr $ S_StrTab str strings (fromIntegral len)

  pure ptr

freeSectionHeader :: Ptr (C_ElfSectionHeader n) -> IO ()
freeSectionHeader ptr = do
  peek ptr >>= \ case
    S_Null                  -> pure ()
    S_ProgBits n d flags dl -> do
      free n
      free d
    S_Rela n syms symsl     -> do
      free n
      mapM_ freeRelocationSymbol =<< peekArray (fromIntegral symsl) syms
      free syms
    S_NoBits n space flags  -> do
      free n
    S_SymTab n syms symsl   -> do
      free n
      mapM_ freeElfSymbol =<< peekArray (fromIntegral symsl) syms
      free syms
    S_StrTab n str strl     -> do
      free n
      free str

  free ptr





-- | Splits a list into components delimited by separators,
-- where the predicate returns True for a separator element.  The
-- resulting components do not contain the separators.  Two adjacent
-- separators result in an empty component in the output.
--
-- > split (== 'a') "aabbaca" == ["","","bb","c",""]
-- > split (== 'a') ""        == [""]
-- > split (== ':') "::xyz:abc::123::" == ["","","xyz","abc","","123","",""]
-- > split (== ',') "my,list,here" == ["my","list","here"]
split :: (a -> Bool) -> [a] -> [[a]]
split f [] = [[]]
split f (x:xs) | f x = [] : split f xs
split f (x:xs) | y:ys <- split f xs = (x:y) : ys
