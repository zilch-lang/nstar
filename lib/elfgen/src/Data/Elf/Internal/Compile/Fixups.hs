{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Elf.Internal.Compile.Fixups
( FixupEnvironment(..), Fixup
, SectionAList, SegmentAList, SectionByName, SymbolsAList
, runFixup
  -- * All fixup steps
, allFixes, fixupHeaderCount, fixupShstrtabIndex, fixupHeadersOffsets, fixupPHDREntry, fixupSectionNames
, fixupSectionOffsets, fixupLoadSegments, fixupSymtabStrtabIndex, fixupSymtabShInfo, fixupSymtabOffset
, fixupSymbolNames, fixupSymbolDefs, fixupFuncSymbolAddresses, fixupEntryPoint
) where

import Data.Elf.SectionHeader
import Data.Elf.Internal.SectionHeader
import Data.Elf.ProgramHeader
import Data.Elf.Internal.ProgramHeader
import Data.Elf.Internal.FileHeader
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Control.Monad.State (State, get, put, execState)
import Data.Functor ((<&>))
import Data.Elf.Internal.BusSize (Size(..))
import Data.List (elemIndex, intercalate)
import Data.Elf.Types (Elf_Off, Elf_Addr, Elf_Xword, ReifySize)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word8)
import Unsafe.Coerce (unsafeCoerce)
import Data.Elf.Symbol
import Data.Elf.Internal.Symbol
import Control.Applicative ((<|>))
import Control.Monad (unless)

-- | Associative list between abstract and concrete section header structures.
type SectionAList n = Map (SectionHeader n) (Elf_Shdr n)
-- | Associative list between abstract and concrete program header structures.
type SegmentAList n = Map (ProgramHeader n) (Elf_Phdr n)
-- | Mapping from section names to abstract section headers.
type SectionByName n = Map Text (SectionHeader n)
-- | Associative list between abstract and concrete symbol structures.
type SymbolsAList n = Map (ElfSymbol n) (Elf_Sym n)

-- | The fixup environment, containing all sections to be fixed up.
data FixupEnvironment (n :: Size)
  = FixupEnv
      (Elf_Ehdr n)         -- ^ The ELF file header
      (SectionAList n)     -- ^ An association list associating abstract and concrete section headers
      (SectionByName n)    -- ^ All segments mapped by their respective names
      (SegmentAList n)     -- ^ An association from abstract to concrete segment headers
      (SymbolsAList n)     -- ^ An association from abstract to concrete symbol structures
      ByteString           -- ^ All generated binary data unrelated to headers (for example the content of the @.shstrtab@ section)

type Fixup (n :: Size) a = ReifySize n => State (FixupEnvironment n) a

defaultStartAddress :: ReifySize n => Elf_Addr n
defaultStartAddress = 0x400000

-- | Run a given fixup step (or multiple) with the given initial environment.
runFixup :: ReifySize n => Fixup n a -> FixupEnvironment n -> FixupEnvironment n
runFixup = execState

-- | Contains all fixes to do.
allFixes :: forall (n :: Size). Fixup n ()
allFixes = do
  FixupEnv fileHeader sects sectsNames _ syms gen <- get
  let isExec = e_type fileHeader == et_exec @n

  unless isExec do
    put (FixupEnv fileHeader sects sectsNames mempty syms gen)

  fixupHeaderCount
  fixupSymtabStrtabIndex
  fixupShstrtabIndex
  fixupHeadersOffsets
  fixupPHDREntry
  fixupSectionNames
  fixupSectionOffsets
  fixupLoadSegments
  fixupSymtabShInfo
  fixupSymtabOffset
  fixupSymbolNames
  fixupSymbolDefs
  fixupFuncSymbolAddresses
  fixupEntryPoint

-- | A fix for headers count in the ELF file header (fields 'e_phnum' and 'e_shnum').
fixupHeaderCount :: Fixup n ()
fixupHeaderCount = do
  FixupEnv fileHeader sects sectsNames segs syms gen <- get
  let newHeader = fileHeader
        { e_phnum = fromIntegral (Map.size segs)
        , e_shnum = fromIntegral (Map.size sects) }
  put (FixupEnv newHeader sects sectsNames segs syms gen)

-- | Fixes the @.shstrtab@ section index in the file header.
fixupShstrtabIndex :: Fixup n ()
fixupShstrtabIndex = do
  FixupEnv fileHeader sects sectsNames segs syms gen <- get
  let Just e_shstrtabndx  = elemIndex ".shstrtab" (getName <$> Map.keys sects)
      newHeader           = fileHeader
        { e_shstrndx = fromIntegral e_shstrtabndx }
  put (FixupEnv newHeader sects sectsNames segs syms gen)
 where
   getName SNull             = ""
   getName (SProgBits n _ _) = n
   getName (SNoBits n _ _)   = n
   getName (SStrTab n _)     = n
   getName (SSymTab n _)     = n

-- | Fixes the @.strtab@ section index in the @.symtab@ section field 'sh_link'.
fixupSymtabStrtabIndex :: Fixup n ()
fixupSymtabStrtabIndex = do
  FixupEnv fileHeader sects sectsNames segs syms gen <- get
  let Just e_strtabndx  = elemIndex ".strtab" (getName <$> Map.keys sects)
      Just symtab       = Map.lookup ".symtab" sectsNames
      symtabSect        = Map.lookup symtab sects <&> \ s ->
        s { sh_link = fromIntegral e_strtabndx }
      newSects          = Map.update (const symtabSect) symtab sects

  put (FixupEnv fileHeader newSects sectsNames segs syms gen)

getName :: SectionHeader n -> String
getName SNull             = ""
getName (SProgBits n _ _) = n
getName (SNoBits n _ _)   = n
getName (SStrTab n _)     = n
getName (SSymTab n _)     = n

-- | Fixes (sections/segments) headers offsets in the file header.
fixupHeadersOffsets :: forall (n :: Size). Fixup n ()
fixupHeadersOffsets = do
  FixupEnv fileHeader sects sectsNames segs syms gen <- get
  let phnum     = fromIntegral (e_phnum fileHeader)
      phentsize = fromIntegral (e_phentsize fileHeader)
      phoff     = fromIntegral (e_ehsize fileHeader)    -- we want to have program headers right after the file header
      shoff     = phoff + phnum * phentsize             -- and section headers right after program headers
      isExec    = e_type fileHeader == et_exec @n
  let newHeader = fileHeader
        { e_shoff = shoff
        , e_phoff = if isExec then phoff else 0x0 }

  put (FixupEnv newHeader sects sectsNames segs syms gen)

-- | Fixes the @LOAD@ segment corresponding to the @PHDR@ segment
--
--   As per @readelf@, we need to have a @LOAD@ segment that loads /at least/ the @PHDR@ in a read-only
--   memory segment.
fixupPHDREntry :: Fixup n ()
fixupPHDREntry = do
  FixupEnv fileHeader sects sectsNames segs syms gen <- get

  let phoff    = fromIntegral (e_phoff fileHeader)
      phdrSize = fromIntegral (e_phnum fileHeader) * fromIntegral (e_phentsize fileHeader)
  let phdr = Map.lookup PPhdr segs <&>
        \ p -> p { p_offset = phoff
                 , p_filesz = phdrSize
                 , p_memsz = phdrSize }
  let phdrFileSize = phdrSize + fromIntegral phoff
  let phdrLoad = Map.lookup (PLoad (section "PHDR") pf_r) segs <&>
        \ p -> p { p_offset = 0x0
                 , p_filesz = phdrFileSize
                 , p_memsz = phdrFileSize }
  let newSegs = Map.update (const phdr) PPhdr $
                Map.update (const phdrLoad) (PLoad (section "PHDR") pf_r) $
                segs

  put (FixupEnv fileHeader sects sectsNames newSegs syms gen)

-- | Fixes every section name index (field 'sh_name') depending on how data is laid out
--   in the @.shstrtab@ section.
fixupSectionNames :: Fixup n ()
fixupSectionNames = do
  FixupEnv fileHeader sects sectsNames segs syms gen <- get

  let names = Map.keys sectsNames
  let newSects = Map.fromList $ names <&> \ n ->
        let index = fetchStringIndex n names
            Just s = Map.lookup n sectsNames
            Just sh = Map.lookup s sects <&> \ sh ->
              sh { sh_name = fromIntegral index }
        in (s, sh)
  let newSectsWithUnmodified = Map.union newSects sects

  put (FixupEnv fileHeader newSectsWithUnmodified sectsNames segs syms gen)

fetchStringIndex :: Text -> [Text] -> Int
fetchStringIndex = goFetch 0
 where
   goFetch :: Int -> Text -> [Text] -> Int
   goFetch n str [] = 0
   goFetch n str (x:xs)
     | str == x  = n + 1
     | otherwise = goFetch (n + 1 + Text.length x) str xs

-- | Inserts binary data from a section (if it contains any) directly into the environment
--   and updates the fields 'sh_addr', 'sh_offset' and 'sh_size'.
fixupSectionOffsets :: forall (n :: Size). Fixup n ()
fixupSectionOffsets = do
  FixupEnv fileHeader sects sectsNames segs syms gen <- get

  let startSects = e_shoff fileHeader
      sectsCount = fromIntegral (e_shnum fileHeader)
      sectsSize  = fromIntegral (e_shentsize fileHeader)
      endSects   = startSects + sectsCount * sectsSize
      isExec    = e_type fileHeader == et_exec @n
  let startPhys = endSects

  let (newGen, newSects) = generateBinFromSectionsStartingAt @n (Map.toList sects) startPhys isExec
  let newGenBin = gen <> newGen
      newSectsModif = Map.union newSects sects

  put (FixupEnv fileHeader newSectsModif sectsNames segs syms newGenBin)
 where
   generateBinFromSectionsStartingAt :: forall (n :: Size).
                                        ReifySize n
                                     => [(SectionHeader n, Elf_Shdr n)]        -- ^ mappings unifying sections
                                     -> Elf_Off n                              -- ^ starting file offset
                                     -> Bool                                   -- ^ Is the file supposed to be executable?
                                     -> (ByteString, Map (SectionHeader n) (Elf_Shdr n))
   generateBinFromSectionsStartingAt [] _ _                      = (mempty, mempty)
   generateBinFromSectionsStartingAt (s@(sh, shd):ss) off isExec =
     let (newOff, sectBin, newSects) = case s of
           (SNull, _)           -> (off, mempty, Map.singleton sh shd)
           (SProgBits _ d _, _) ->
             let size       = fromIntegral (length d)
                 packed     = BS.pack d
                 addr       = (if isExec then defaultStartAddress @n + fromIntegral off else 0)
                 updatedShd = shd { sh_offset = off, sh_addr = addr, sh_size = size }
             in (off + fromIntegral size, packed, Map.singleton sh updatedShd)
           (SNoBits _ s _, _)   ->
             let addr       = (if isExec then defaultStartAddress @n + fromIntegral off else 0)
                 updatedShd = shd { sh_offset = off, sh_addr = addr, sh_size = fromIntegral s }
             in (off, mempty, Map.singleton sh updatedShd)
           (SStrTab _ s, _)     ->
             let content    = c2w <$> ('\0' : (intercalate "\0" s <> "\0"))
                 size       = fromIntegral (length content)
                 packed     = BS.pack content
                 updatedShd = shd { sh_offset = off, sh_size = size }
             in (off + fromIntegral size, packed, Map.singleton sh updatedShd)
           (SSymTab _ _, _)     -> (off, mempty, Map.singleton sh shd)
               -- NOTE: we do not generate anything from a symtab, because we want to handle symbol compilation
               --       later, because we lack some information at the current moment (like the data size pointed by a symbol
               --       or even the section it is in)

         (binGen, sects) = generateBinFromSectionsStartingAt ss newOff isExec
     in (sectBin <> binGen, Map.union newSects sects)

   c2w :: Char -> Word8
   c2w = unsafeCoerce

-- | Fixes all @LOAD@ segments with correct addresses, sizes and offsets (fields 'p_vaddr', 'p_paddr', 'p_filesz', 'p_memsz' and 'p_offset').
fixupLoadSegments :: forall (n :: Size). Fixup n ()
fixupLoadSegments = do
  FixupEnv fileHeader sects sectsNames segs syms gen <- get

  let startSects = e_shoff fileHeader
      sectsCount = fromIntegral (e_shnum fileHeader)
      sectsSize  = fromIntegral (e_shentsize fileHeader)
      endSects   = startSects + sectsCount * sectsSize

      startOff   = endSects + fromIntegral (BS.length gen)
  let (newSegs, newGen)  = fixSegmentsOffsetsAndAddresses @n (Map.toList segs) startOff sects sectsNames
      newSegsWithUnmodif = Map.union newSegs segs

  put (FixupEnv fileHeader sects sectsNames newSegsWithUnmodif syms (gen <> newGen))
 where
   fixSegmentsOffsetsAndAddresses :: forall (n :: Size).
                                     ReifySize n
                                  => [(ProgramHeader n, Elf_Phdr n)]      -- ^ mappings unifying segments
                                  -> Elf_Off n                            -- ^ initial offset (end of file)
                                  -> SectionAList n
                                  -> SectionByName n
                                  -> (Map (ProgramHeader n) (Elf_Phdr n), ByteString)
   fixSegmentsOffsetsAndAddresses [] _ _ _                              = (mempty, mempty)
   fixSegmentsOffsetsAndAddresses (s@(ph, phd):ss) off sects sectsNames =
     let (newOff, segBin, newSegs) = case s of
           (PPhdr, _)                  -> (off, mempty, Map.singleton ph phd)
           (PNull, _)                  -> (off, mempty, Map.singleton ph phd)
           (PLoad (Left "PHDR") _, _)  -> (off, mempty, Map.singleton ph phd)
           (PLoad (Left section) _, _) ->
             let Just sect = Map.lookup (Text.pack section) sectsNames
                 Just shdr = Map.lookup sect sects
             in
             let sectSize   = sh_size shdr
                 sectAddr   = sh_addr shdr
                 sectOff    = sh_offset shdr
                 updatedPhd = phd { p_offset = sectOff, p_vaddr = sectAddr, p_paddr = sectAddr, p_filesz = sectSize, p_memsz = sectSize }
             in (off, mempty, Map.singleton ph updatedPhd)
           (PLoad (Right binDat) _, _) ->
             let packed     = BS.pack binDat
                 size       = fromIntegral (BS.length packed)
                 addr       = defaultStartAddress @n + fromIntegral off
                 updatedPhd = phd { p_offset = off, p_vaddr = addr, p_paddr = addr, p_filesz = size, p_memsz = size }
             in (off + fromIntegral size, packed, Map.singleton ph updatedPhd)
           (PInterp path _, _)         ->
             error "not yet implemented: fixup INTERP [PATH]"

         (segs, gen) = fixSegmentsOffsetsAndAddresses ss newOff sects sectsNames
     in (Map.union newSegs segs, segBin <> gen)

-- | Fixes the field 'sh_info' of the section @.symtab@ with the number of symbols in the file - 1.
fixupSymtabShInfo :: Fixup n ()
fixupSymtabShInfo = do
  FixupEnv fileHeader sects sectsNames segs syms gen <- get

  let Just symtab = Map.lookup ".symtab" sectsNames
      symtabSect  = Map.lookup symtab sects <&> \ s ->
        s { sh_info = fromIntegral (Map.size syms) - 1 }
                                     --            ^^^ DO NOT COUNT THE NULL ENTRY
                                     --                else `ld` is not able to figure out symbols
                                     --                and will spit out errors like:
                                     --                > undefined reference to `main'
      newSects    = Map.update (const symtabSect) symtab sects
  put (FixupEnv fileHeader newSects sectsNames segs syms gen)

-- | Fixes the @.symtab@ section offset.
--
--   __NOTE:__ This is handled differently than other section because we need to know
--             all data generated before trying to write the symbol table (function sizes will be unknown otherwise
--             and most probably unfixable if written before).
fixupSymtabOffset :: Fixup n ()
fixupSymtabOffset = do
  FixupEnv fileHeader sects sectsNames segs syms gen <- get

  let headerSize   = fromIntegral $ e_ehsize fileHeader
      segmentHSize = fromIntegral $ e_phnum fileHeader * e_phentsize fileHeader
      sectionHSize = fromIntegral $ e_shnum fileHeader * e_shentsize fileHeader
  let initialSize  = fromIntegral (BS.length gen) + headerSize + segmentHSize + sectionHSize
       --            ^^^ start at the end of the file
  let Just symtab = Map.lookup ".symtab" sectsNames
      symtabSect  = Map.lookup symtab sects <&> \ s ->
        s { sh_offset = initialSize, sh_size = fromIntegral (sh_entsize s) * fromIntegral (sh_info s + 1) }
      newSects    = Map.update (const symtabSect) symtab sects

  put (FixupEnv fileHeader newSects sectsNames segs syms gen)

-- | Replaces symbols names with the corresponding indices in the @.strtab@ section.
fixupSymbolNames :: Fixup n ()
fixupSymbolNames = do
  FixupEnv fileHeader sects sectsNames segs syms gen <- get

  let Just (SStrTab _ c) = Map.lookup ".strtab" sectsNames
      newSyms            = flip Map.mapWithKey syms \ (ElfSymbol n _ _ _) sy ->
        let index =
              if n /= ""
              then fetchStringIndex (Text.pack n) (Text.pack <$> c)
              else 0x0
        in sy { st_name = fromIntegral index }

  put (FixupEnv fileHeader sects sectsNames segs newSyms gen)

-- | Sets 'st_shndx' to
--
--   * the @.text@ section index for all function symbols.
--
--   * the @.data@ section index for all object symbols.
--
--   * the section pointed to for all section symbols.
fixupSymbolDefs :: Fixup n ()
fixupSymbolDefs = do
  FixupEnv fileHeader sects sectsNames segs syms gen <- get

  -- FIXME: we do not have a way of identifying whether functions (more generally symbols)
  -- are bound externally (if yes, we should set 'st_shndx' to @0x0@) or internally (if yes,
  -- we have to set 'st_shndx' to either the @.text@ section index or the @.data@/@.rodata@ section index).
  -- We will consider that all functions are internally bound to begin with.

  let Just textIndex = elemIndex ".text" (getName <$> Map.keys sects)
      Just dataIndex = elemIndex ".data" (getName <$> Map.keys sects)
      newSyms        = flip Map.mapWithKey syms \ (ElfSymbol _ ty _ _) st ->
        case ty of
          ST_NoType    -> st
          ST_Func _    -> st { st_shndx = fromIntegral textIndex }
          ST_Object    -> st { st_shndx = fromIntegral dataIndex }
          ST_Section n ->
            let Just sectIndex = elemIndex n (getName <$> Map.keys sects)
            in st { st_shndx = fromIntegral sectIndex }
          _            -> st

  put (FixupEnv fileHeader sects sectsNames segs newSyms gen)

-- | Fixes function symbol addresses (field 'st_value') and code object sizes (field 'st_size').
fixupFuncSymbolAddresses :: forall (n :: Size). Fixup n ()
fixupFuncSymbolAddresses = do
  FixupEnv fileHeader sects sectsNames segs syms gen <- get

  let Just textSect = Map.lookup ".text" sectsNames
      Just text     = Map.lookup textSect sects
      startAddr     = if isExec then sh_addr text else 0x0
      textSize      = sh_size text

      isExec        = e_type fileHeader == et_exec @n

      newSyms       = Map.fromList (fixSymbolAddressesBy2 startAddr textSize (Map.toList syms))
             -- we skip everything that is not a function

  put (FixupEnv fileHeader sects sectsNames segs newSyms gen)
 where
   fixSymbolAddressesBy2 :: Elf_Addr n -> Elf_Xword n -> [(ElfSymbol n, Elf_Sym n)] -> [(ElfSymbol n, Elf_Sym n)]
   fixSymbolAddressesBy2 _ _ []                                                                            = []
   fixSymbolAddressesBy2 startAddr endOff [(e@(ElfSymbol _ ty _ _), st)]                                   = case ty of
     ST_Func off ->
       let symbolSize = endOff - fromIntegral off
           symbolAddr = startAddr + fromIntegral off
       in [(e, st { st_value = symbolAddr, st_size = symbolSize })]
     _           -> [(e, st)]
   fixSymbolAddressesBy2 startAddr endOff (e1@(ElfSymbol _ ty1 _ _, st1):e2@(ElfSymbol _ ty2 _ _, st2):es) = case (ty1, ty2) of
     (ST_Func off1, ST_Func off2) ->
       -- @off2@ is the starting point of the second function;
       -- so our first function goes from @off1@ to @off2@.
       let symbolSize = fromIntegral $ off2 - off1
           symbolAddr = startAddr + fromIntegral off1
       in (fst e1, st1 { st_value = symbolAddr, st_size = symbolSize }) : fixSymbolAddressesBy2 startAddr endOff (e2:es)
     (ST_Func off1, _)            -> fixSymbolAddressesBy2 startAddr endOff [e1] <> fixSymbolAddressesBy2 startAddr endOff (e2:es)
     (_, ST_Func off2)            -> e1 : fixSymbolAddressesBy2 startAddr endOff (e2:es)
     (_, _)                       -> e1 : e2 : fixSymbolAddressesBy2 startAddr endOff es

-- | Replace the 'e_entry' file header field with the address of the 'main' function, if there is one.
--   Else leaves the header untouched.
fixupEntryPoint :: Fixup n ()
fixupEntryPoint = do
  FixupEnv fileHeader sects sectsNames segs syms gen <- get

  let mainSym   = Map.foldrWithKey (\ (ElfSymbol n _ _ _) st sym -> sym <|> if n == "_start" then Just st else Nothing) Nothing syms
  let newHeader = case mainSym of
        Nothing -> fileHeader
        Just ms -> fileHeader { e_entry = st_value ms }

  put (FixupEnv newHeader sects sectsNames segs syms gen)
