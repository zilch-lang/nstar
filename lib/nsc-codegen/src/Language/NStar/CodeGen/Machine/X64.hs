{-# LANGUAGE BinaryLiterals #-}

module Language.NStar.CodeGen.Machine.X64 (compileX64) where

import Language.NStar.CodeGen.Compiler
import Language.NStar.Syntax.Core hiding (Label, Instruction(..))
import Language.NStar.Typechecker.Core
import Language.NStar.CodeGen.Machine.Internal.Intermediate (TypeContext, InterOpcode(..))
import Language.NStar.CodeGen.Machine.Internal.X64.SIB
import Language.NStar.CodeGen.Machine.Internal.X64.ModRM
import Language.NStar.CodeGen.Machine.Internal.X64.RegisterEncoding (registerNumber)
import Data.Located (unLoc, Located(..))
import Language.NStar.CodeGen.Errors
import Control.Monad.Except (throwError)
import Internal.Error (internalError)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Word (Word8)
import Data.Bits ((.&.), shiftR)
import Control.Monad.Writer (tell)
import Data.Text (Text)
import Data.Binary.Put
import qualified Data.ByteString.Lazy as BS (unpack)
import Data.Char (ord)
import Data.Bits ((.&.), (.|.), shiftL)
import Debug.Trace (traceShow)
import Data.Bifunctor (second)
import Data.Elf (RelocationType(..))
import Language.NStar.CodeGen.Machine.X64.Jmp (compileJmp)
import Language.NStar.CodeGen.Machine.X64.Nop (compileNop)
import Language.NStar.CodeGen.Machine.X64.Mv (compileMv)
import Language.NStar.CodeGen.Machine.X64.Add (compileAdd)
import Language.NStar.CodeGen.Machine.X64.Sub (compileSub)
import Language.NStar.CodeGen.Machine.X64.Expression (int32, int64, compileConstantX64)
import Language.NStar.CodeGen.Machine.X64.Salloc (compileSalloc)
import Language.NStar.CodeGen.Machine.X64.Sfree (compileSfree)
import Language.NStar.CodeGen.Machine.X64.Sld (compileSld)

compileX64 :: TypedProgram -> Compiler ()
compileX64 prog@(TProgram (TData dataSect :@ _) _ _ _) = do
  intermediateOpcodes <- compileInterX64 prog
  generateDataX64 dataSect
  fixupAddressesX64 intermediateOpcodes

compileInterX64 :: TypedProgram -> Compiler [InterOpcode]
compileInterX64 (TProgram (TData _ :@ _) (TROData _ :@ _) (TUData _ :@ _) (TCode stmts :@ _)) =
  mconcat <$> mapM (compileStmtInterX64 . unLoc) stmts

compileStmtInterX64 :: TypedStatement -> Compiler [InterOpcode]
compileStmtInterX64 (TLabel name is) = (Label (unLoc name) :) . mconcat <$> mapM compileStmtInterX64 is
compileStmtInterX64 (TInstr i _ _ _) = compileInstrInterX64 (unLoc i)

compileInstrInterX64 :: TypedInstruction -> Compiler [InterOpcode]
compileInstrInterX64 (JMP dst)          = compileJmp (unLoc dst)
compileInstrInterX64 (NOP)              = compileNop
compileInstrInterX64 (MV src dst)       = compileMv (unLoc src) (unLoc dst)
compileInstrInterX64 (SALLOC n)         = compileSalloc (unLoc n)
compileInstrInterX64 (SFREE n)          = compileSfree (unLoc n)
compileInstrInterX64 (SLD n r)          = compileSld (unLoc n) (unLoc r)
compileInstrInterX64 i                  = internalError $ "not yet supported: compileInterInstrX64 " <> show i

---------------------------------------------------------------------------------------------------------------------

fixupAddressesX64 :: [InterOpcode] -> Compiler ()
fixupAddressesX64 os = fixupAddressesX64Internal (findLabelsAddresses os) os 0
 where
   fixupAddressesX64Internal :: Map Text Integer -> [InterOpcode] -> Integer -> Compiler ()
   fixupAddressesX64Internal labelsAddresses [] _             = tell (MInfo mempty (second Function <$> Map.assocs labelsAddresses) mempty mempty)
   fixupAddressesX64Internal labelsAddresses (Byte b:os) i    = tell (MInfo [b] mempty mempty mempty) *> fixupAddressesX64Internal labelsAddresses os (i + 1)
   fixupAddressesX64Internal labelsAddresses (Label n:os) i   = fixupAddressesX64Internal labelsAddresses os i
   fixupAddressesX64Internal labelsAddresses (Jump n:os) i    = do
     let addr = maybe (internalError $ "Label " <> show n <> " not found during codegen.") (int32 . (subtract (i + 4))) (Map.lookup n labelsAddresses)
     tell (MInfo addr mempty mempty mempty) *> fixupAddressesX64Internal labelsAddresses os (i + 4)
   -- TODO: implement this
   fixupAddressesX64Internal labelsAddresses (Symbol32 s o:os) i  = do
     tell (MInfo (int32 0x0) mempty mempty [RelocText s ".data" o i R_x86_64_32s])
                                               --       ^^^^^^^ FIXME: do not hardcode origin section
     fixupAddressesX64Internal labelsAddresses os (i + 4)
   fixupAddressesX64Internal labelsAddresses (Symbol64 s:os) i = do
     tell (MInfo (int64 0x0) mempty mempty [RelocText s ".data" 0x0 i R_x86_64_64])
                                              --        ^^^^^^^ FIXME: do not hardcode origin section
     fixupAddressesX64Internal labelsAddresses os (i + 8)

findLabelsAddresses :: [InterOpcode] -> Map Text Integer
findLabelsAddresses = findLabelsAddressesInternal 0
  where
    findLabelsAddressesInternal :: Integer -> [InterOpcode] -> Map Text Integer
    findLabelsAddressesInternal n []                = mempty
    findLabelsAddressesInternal n (Byte _:os)       = findLabelsAddressesInternal (n + 1) os
    findLabelsAddressesInternal n (Label l:os)      = Map.insert l n (findLabelsAddressesInternal n os)
    findLabelsAddressesInternal n (Jump l:os)       = findLabelsAddressesInternal (n + 4) os
    findLabelsAddressesInternal n (Symbol32 s o:os) = findLabelsAddressesInternal (n + 4) os
    findLabelsAddressesInternal n (Symbol64 s:os)   = findLabelsAddressesInternal (n + 8) os

--------------------------------------------------------------------------------------------------------------------

generateDataX64 :: [Located Binding] -> Compiler ()
generateDataX64 = generateDataX64Internal 0
  where
    generateDataX64Internal :: Integer -> [Located Binding] -> Compiler ()
    generateDataX64Internal _ [] = pure ()
    generateDataX64Internal off ((Bind (name :@ _) _ (val :@ _) :@ _) : bs) = do
      let cst = compileConstantX64 val
      tell (MInfo mempty mempty (DataTable [(name, off)] cst) mempty)
      generateDataX64Internal (off + fromIntegral (length cst)) bs
