{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Data.Elf.Internal.Compile (unabstract, CompileFor) where

import Data.Elf.Internal.BusSize (Size (..))
import qualified Data.Elf.Internal.Object as Internal
import Data.Elf.Object
import Data.Elf.ProgramHeader (ProgramHeader (PLoad, PPhdr), pf_r, section)
import Data.Elf.SectionHeader (SectionHeader (..))
import Data.Elf.Symbol
import Data.Elf.Types
import Data.Functor ((<&>))
import Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import qualified Data.HashMap.Strict.InsOrd as Map
import Data.Kind (Type)
import Data.List (sort)
import Data.Maybe (mapMaybe)
import Foreign.ForeignPtr (addForeignPtrFinalizer, mallocForeignPtr, withForeignPtr)
import Foreign.Ptr (FunPtr, Ptr)

foreign import ccall unsafe "compile_x64"
  c_compileX64 :: Ptr (C_ElfObject 'S64) -> Ptr (Internal.C_Object 'S64) -> IO ()

foreign import ccall unsafe "&free_elf64_object"
  freeInternalObjectX64 :: FunPtr (Ptr (Internal.C_Object 'S64) -> IO ())

--foreign import ccall unsafe "compile_x86"
--  c_compileX86 :: Ptr (ElfObject S32) -> ForeignPtr (Internal.Object S32) -> IO ()

-- | Transforms an abstract ELF object into a concrete ELF object.
--
--   This is essentially a simple alias on 'compileFor' specialized for ELF objects.
--
--   >>> unabstract = compileFor
unabstract ::
  ( ReifySize n,
    CompileFor n ElfObject Internal.Object
  ) =>
  ElfObject n ->
  IO (Internal.Object n)
unabstract = compileFor

class CompileFor (n :: Size) (a :: Size -> Type) (b :: Size -> Type) where
  -- | Compiles a value of type @a@ into a value of type @b@ parameterized by the target architecture bus size @n@.
  compileFor :: a n -> IO (b n)

instance CompileFor 'S64 ElfObject Internal.Object where
  compileFor obj = do
    aObj <- newObject (mkAbstractObject @'S64 obj)

    obj <- mallocForeignPtr @(Internal.C_Object 'S64)
    addForeignPtrFinalizer freeInternalObjectX64 obj
    cObj <- withForeignPtr obj \ptr -> do
      c_compileX64 aObj ptr
      Internal.peekObject ptr

    freeObject aObj

    pure cObj

fetchSectionNamesFrom :: [SectionHeader n] -> InsOrdHashMap String (SectionHeader n)
fetchSectionNamesFrom = Map.fromList . fmap f
  where
    f s@SNull = ("", s)
    f s@(SProgBits n _ _) = (n, s)
    f s@(SNoBits n _ _) = (n, s)
    f s@(SStrTab n _) = (n, s)
    f s@(SSymTab n _) = (n, s)
    f s@(SRela n _) = (n, s)

fetchSymbols :: [SectionHeader n] -> [ElfSymbol n]
fetchSymbols = mconcat . mapMaybe f
  where
    f SNull = Nothing
    f (SProgBits _ _ _) = Nothing
    f (SNoBits _ _ _) = Nothing
    f (SStrTab _ _) = Nothing
    f (SSymTab _ syms) = Just syms
    f (SRela _ _) = Nothing

sectionsAsSymbols :: [String] -> [ElfSymbol n]
sectionsAsSymbols = fmap intoSymbol . filter allowedInSymbolTable
  where
    intoSymbol name = ElfSymbol "" (ST_Section name) SB_Local SV_Default

    allowedInSymbolTable ".text" = True
    allowedInSymbolTable ".data" = True
    allowedInSymbolTable ".bss" = True
    allowedInSymbolTable _ = False

mkAbstractObject :: forall (n :: Size). ElfObject n -> ElfObject n
mkAbstractObject ElfObject {..} =
  let sectByNames = fetchSectionNamesFrom (SNull : sections)

      segs = PPhdr : PLoad (section "PHDR") pf_r : segments
      --                             ^^^^ Special identifier, to refer to the PHDR segment

      symbols = sort $ ElfSymbol "" ST_NoType SB_Local SV_Default : sectionsAsSymbols (Map.keys sectByNames) <> fetchSymbols sections

      allSectionNames = Map.keys sectByNames <> [".shstrtab", ".strtab", "\0"]
      allSymbolNames = filter (/= "") $ symbols <&> \(ElfSymbol n _ _ _) -> n

      sects =
        Map.insert ".shstrtab" (SStrTab ".shstrtab" $ allSectionNames <> ["\0"]) $
          Map.insert ".strtab" (SStrTab ".strtab" $ allSymbolNames <> ["\0"]) $
            Map.insert
              ".symtab"
              (SSymTab ".symtab" symbols)
              sectByNames

      abstractObject = ElfObject fileHeader segs (Map.elems sects)
   in abstractObject
