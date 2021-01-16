{-# LANGUAGE BangPatterns #-}

module Data.Elf.Symbol
( ElfSymbol(..)
, SymbolType(..)
, SymbolBinding(..)
, SymbolVisibility(..)
, RelocationSymbol(..)
, RelocationOrigin(..)
, RelocationType(..)
  -- * C bits
, C_ElfSymbol
, newElfSymbol, peekElfSymbol, freeElfSymbol
, C_RelocationSymbol
, newRelocationSymbol, peekRelocationSymbol, freeRelocationSymbol
) where

import Data.Elf.Types
import Data.Elf.Internal.BusSize (Size)
import Foreign.C.String (CString, newCString, peekCString)
import Foreign.C.Types (CULong, CInt)
import Foreign.Storable (Storable(..))
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Marshal.Alloc (malloc, free)
import Data.Word (Word64)

#include "symbol.h"

-- | Symbol table entry
data ElfSymbol (n :: Size)
  = ElfSymbol
      String           -- ^ Symbol name
      SymbolType       -- ^ Symbol type
      SymbolBinding    -- ^ Symbol binding
      SymbolVisibility -- ^ Symbol visibility
  deriving (Eq, Ord)

-- | Symbol type
data SymbolType
  = ST_NoType          -- ^ Symbol type is unspecified
  | ST_Object Integer  -- ^ Symbol is a data object
  | ST_Func Integer    -- ^ Symbol is a code object
  | ST_Section String  -- ^ Symbol associated with a section
  | ST_File String     -- ^ Symbol's name is file name
  | ST_Common          -- ^ Symbol is a common data object
  | ST_TLS             -- ^ Symbol is a thread-local data object
  deriving (Eq, Ord)

-- | Symbol binding type
{#enum symbol_binding as SymbolBinding {SB_LOCAL as SB_Local, SB_GLOBAL as SB_Global, SB_WEAK as SB_Weak} deriving (Eq, Ord)#}

-- | Symbol visibility specification
{#enum symbol_visibility as SymbolVisibility {SV_DEFAULT as SV_Default, SV_INTERNAL as SV_Internal, SV_HIDDEN as SV_Hidden, SV_PROTECTED as SV_Protected} deriving (Eq, Ord)#}

{#enum symbol_type_ as C_SymbolType' {ST_NOTYPE as STT_NoType, ST_OBJECT as STT_Object, ST_FUNC as STT_Func, ST_SECTION as STT_Section, ST_FILE as STT_File, ST_COMMON as STT_Common, ST_TLS as STT_TLS}#}

data C_SymbolType
  = STNoType
  | STObject CULong
  | STFunc CULong
  | STSection CString
  | STFile CString
  | STCommon
  | STTLS

data C_ElfSymbol (n :: Size)
  = C_ElfSymbol
      CString
      (Ptr C_SymbolType)
      SymbolBinding
      SymbolVisibility

instance Storable C_SymbolType where
  sizeOf _ = {#sizeof symbol_type#}
  alignment _ = {#alignof symbol_type#}
  peek ptr = (toEnum . fromIntegral <$> {#get struct symbol_type->type#} ptr) >>= \ case
    STT_NoType  -> pure STNoType
    STT_Object  -> STObject <$> {#get struct symbol_type->data.st_object.offset#} ptr
    STT_Func    -> STFunc <$> {#get struct symbol_type->data.st_func.offset#} ptr
    STT_Section -> STSection <$> {#get struct symbol_type->data.st_section.name#} ptr
    STT_File    -> STFile <$> {#get struct symbol_type->data.st_file.name#} ptr
    STT_Common  -> pure STCommon
    STT_TLS     -> pure STTLS
  poke ptr STNoType = do
    {#set struct symbol_type->type#} ptr (fromIntegral $ fromEnum STT_NoType)
  poke ptr STCommon = do
    {#set struct symbol_type->type#} ptr (fromIntegral $ fromEnum STT_Common)
  poke ptr STTLS = do
    {#set struct symbol_type->type#} ptr (fromIntegral $ fromEnum STT_TLS)
  poke ptr (STObject off) = do
    {#set struct symbol_type->type#} ptr (fromIntegral $ fromEnum STT_Object)
    {#set struct symbol_type->data.st_object.offset#} ptr off
  poke ptr (STFunc off) = do
    {#set struct symbol_type->type#} ptr (fromIntegral $ fromEnum STT_Func)
    {#set struct symbol_type->data.st_func.offset#} ptr off
  poke ptr (STSection s) = do
    {#set struct symbol_type->type#} ptr (fromIntegral $ fromEnum STT_Section)
    {#set struct symbol_type->data.st_section.name#} ptr s
  poke ptr (STFile s) = do
    {#set struct symbol_type->type#} ptr (fromIntegral $ fromEnum STT_File)
    {#set struct symbol_type->data.st_file.name#} ptr s

newSymbolType :: SymbolType -> IO (Ptr C_SymbolType)
newSymbolType typ = do
  ptr <- malloc @C_SymbolType

  case typ of
    ST_NoType     -> poke ptr STNoType
    ST_Object off -> poke ptr $ STObject (fromIntegral off)
    ST_Func off   -> poke ptr $ STFunc (fromIntegral off)
    ST_Section n  -> poke ptr . STSection =<< newCString n
    ST_File n     -> poke ptr . STFile =<< newCString n
    ST_Common     -> poke ptr STCommon
    ST_TLS        -> poke ptr STTLS

  pure ptr

peekSymbolType :: Ptr C_SymbolType -> IO SymbolType
peekSymbolType ptr =
  peek ptr >>= \ case
    STNoType    -> pure ST_NoType
    STCommon    -> pure ST_Common
    STTLS       -> pure ST_TLS
    STObject o  -> pure $ ST_Object (fromIntegral o)
    STFunc o    -> pure $ ST_Func (fromIntegral o)
    STSection n -> ST_Section <$> peekCString n
    STFile n    -> ST_File <$> peekCString n

freeSymbolType :: Ptr C_SymbolType -> IO ()
freeSymbolType ptr = do
  peek ptr >>= \ case
    STNoType    -> pure ()
    STCommon    -> pure ()
    STTLS       -> pure ()
    STObject o  -> pure ()
    STFunc o    -> pure ()
    STSection n -> free n
    STFile n    -> free n
  free ptr

instance Storable (C_ElfSymbol n) where
  sizeOf _ = {#sizeof elf_symbol#}
  alignment _ = {#alignof elf_symbol#}
  peek ptr =
    C_ElfSymbol <$> {#get struct elf_symbol->name#} ptr
                <*> (castPtr <$> {#get struct elf_symbol->type#} ptr)
                <*> (toEnum . fromIntegral <$> {#get struct elf_symbol->binding#} ptr)
                <*> (toEnum . fromIntegral <$> {#get struct elf_symbol->visibility#} ptr)
  poke ptr (C_ElfSymbol n t b v) = do
    {#set struct elf_symbol->name#} ptr n
    {#set struct elf_symbol->type#} ptr (castPtr t)
    {#set struct elf_symbol->binding#} ptr (fromIntegral $ fromEnum b)
    {#set struct elf_symbol->visibility#} ptr (fromIntegral $ fromEnum v)

newElfSymbol :: forall (n :: Size). ElfSymbol n -> IO (Ptr (C_ElfSymbol n))
newElfSymbol (ElfSymbol name typ bind vis) = do
  ptr <- malloc @(C_ElfSymbol n)
  str <- newCString name
  symType <- newSymbolType typ

  poke ptr $ C_ElfSymbol str symType bind vis

  pure ptr

peekElfSymbol :: Ptr (C_ElfSymbol n) -> IO (ElfSymbol n)
peekElfSymbol ptr = do
  C_ElfSymbol str typ bind vis <- peek ptr
  name <- peekCString str
  symType <- peekSymbolType typ
  pure (ElfSymbol name symType bind vis)

freeElfSymbol :: Ptr (C_ElfSymbol n) -> IO ()
freeElfSymbol ptr = do
  C_ElfSymbol str typ bind vis <- peek ptr
  free str
  freeSymbolType typ
  free ptr

------------------------------------------------------------------------------------------------

-- | Relocation table entry
data RelocationSymbol (n :: Size)
  = RelocationSymbol
      RelocationOrigin -- ^ Symbol name
      RelocationType   -- ^ Relocation type
      Integer          -- ^ Relocation offset in section
  deriving (Eq, Ord)

data RelocationOrigin
  -- | Relocating from a section
  = SectionReloc
      String        -- ^ Section name
      Integer       -- ^ Offset from beginning of section
  deriving (Eq, Ord)

-- | Symbol relocation type
{#enum symbol_relocation_type as RelocationType {RT_X86_64_NONE as R_x86_64_None, RT_X86_64_32 as R_x86_64_32, RT_X86_64_32S as R_x86_64_32s, RT_X86_64_64 as R_x86_64_64} deriving (Eq, Ord)#}

{#enum relocation_origin_type as RelocType {ORIGIN_SECTION as OrigSection}#}

data C_RelocationOrigin
  = OriginSection
      CString
      Word64

instance Storable C_RelocationOrigin where
  sizeOf _ = {#sizeof relocation_origin#}
  alignment _ = {#alignof relocation_origin#}
  peek ptr = (toEnum . fromIntegral <$> {#get struct relocation_origin->type#} ptr) >>= \ case
    OrigSection -> OriginSection <$> {#get struct relocation_origin->data.origin_section.section_name#} ptr
                                 <*> (fromIntegral <$> {#get struct relocation_origin->data.origin_section.offset#} ptr)
  poke ptr (OriginSection n o) = do
    {#set struct relocation_origin->type#} ptr (fromIntegral $ fromEnum OrigSection)
    {#set struct relocation_origin->data.origin_section.section_name#} ptr n
    {#set struct relocation_origin->data.origin_section.offset#} ptr (fromIntegral o)

newRelocationOrigin :: RelocationOrigin -> IO (Ptr C_RelocationOrigin)
newRelocationOrigin rel = do
  ptr <- malloc @C_RelocationOrigin
  case rel of
    SectionReloc name offset -> do
      str <- newCString name
      poke ptr $ OriginSection str (fromIntegral offset)
  pure ptr

peekRelocationOrigin :: Ptr C_RelocationOrigin -> IO RelocationOrigin
peekRelocationOrigin ptr = do
  peek ptr >>= \ case
    OriginSection n o -> SectionReloc <$> peekCString n
                                      <*> pure (fromIntegral o)

freeRelocationOrigin :: Ptr C_RelocationOrigin -> IO ()
freeRelocationOrigin ptr = do
  peek ptr >>= \ case
    OriginSection n o -> free n
  free ptr

data C_RelocationSymbol (n :: Size)
  = C_RelocationSymbol
      (Ptr C_RelocationOrigin)
      RelocationType
      Word64

instance Storable (C_RelocationSymbol n) where
  sizeOf _ = {#sizeof elf_relocation_symbol#}
  alignment _ = {#alignof elf_relocation_symbol#}
  peek ptr =
    C_RelocationSymbol <$> (castPtr <$> {#get struct elf_relocation_symbol->origin#} ptr)
                       <*> (toEnum . fromIntegral <$> {#get struct elf_relocation_symbol->reloc_type#} ptr)
                       <*> (fromIntegral <$> {#get struct elf_relocation_symbol->offset#} ptr)
  poke ptr (C_RelocationSymbol orig typ off) = do
    {#set struct elf_relocation_symbol->origin#} ptr (castPtr orig)
    {#set struct elf_relocation_symbol->reloc_type#} ptr (fromIntegral $ fromEnum typ)
    {#set struct elf_relocation_symbol->offset#} ptr (fromIntegral off)

newRelocationSymbol :: forall (n :: Size). RelocationSymbol n -> IO (Ptr (C_RelocationSymbol n))
newRelocationSymbol (RelocationSymbol orig typ off) = do
  ptr <- malloc @(C_RelocationSymbol n)
  origin <- newRelocationOrigin orig
  poke ptr $ C_RelocationSymbol origin typ (fromIntegral off)
  pure ptr

peekRelocationSymbol :: Ptr (C_RelocationSymbol n) -> IO (RelocationSymbol n)
peekRelocationSymbol ptr = do
  C_RelocationSymbol orig typ off <- peek ptr
  origin <- peekRelocationOrigin orig
  pure (RelocationSymbol origin typ (fromIntegral off))

freeRelocationSymbol :: Ptr (C_RelocationSymbol n) -> IO ()
freeRelocationSymbol ptr = do
  C_RelocationSymbol origin typ off <- peek ptr
  freeRelocationOrigin origin
  free ptr
