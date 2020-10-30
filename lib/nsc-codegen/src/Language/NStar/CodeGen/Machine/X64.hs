module Language.NStar.CodeGen.Machine.X64 (compileX64) where

import Language.NStar.CodeGen.Compiler
import Language.NStar.Typechecker.Core
import Language.NStar.CodeGen.Machine.Internal.Intermediate (InterOpcode(..))
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

compileX64 :: TypedProgram -> Compiler ()
compileX64 = (fixupAddressesX64 =<<) . compileInterX64

compileInterX64 :: TypedProgram -> Compiler [InterOpcode]
compileInterX64 (TProgram stmts) = mconcat <$> mapM (compileStmtInterX64 . unLoc) stmts

compileStmtInterX64 :: TypedStatement -> Compiler [InterOpcode]
compileStmtInterX64 (TLabel name) = pure [Label (unLoc name)]
compileStmtInterX64 (TInstr i ts) = compileInstrInterX64 (unLoc i) (unLoc <$> ts)

compileInstrInterX64 :: Instruction -> [Type] -> Compiler [InterOpcode]
compileInstrInterX64 RET []   = pure [Byte 0xC3]
compileInstrInterX64 RET args = internalError $ "Expected [] but got " <> show args <> " as arguments for " <> show RET
compileInstrInterX64 i args   = error $ "not yet implemented: compileInterInstrX64 " <> show i <> " " <> show args

fixupAddressesX64 :: [InterOpcode] -> Compiler ()
fixupAddressesX64 os = fixupAddressesX64Internal (findLabelsAddresses os) os
 where
   fixupAddressesX64Internal :: Map Text Integer -> [InterOpcode] -> Compiler ()
   fixupAddressesX64Internal labelsAddresses []             = pure ()
   fixupAddressesX64Internal labelsAddresses (Byte b:os)    = tell [b] *> fixupAddressesX64Internal labelsAddresses os
   fixupAddressesX64Internal labelsAddresses (Label n:os)   = fixupAddressesX64Internal labelsAddresses os
   fixupAddressesX64Internal labelsAddresses (Jump n:os)    =
     let addr = maybe (internalError $ "Label " <> show n <> " not found during codegen.") to4BLittleEndian (Map.lookup n labelsAddresses)
     in tell addr *> fixupAddressesX64Internal labelsAddresses os

to4BLittleEndian :: Integer -> [Word8]
to4BLittleEndian n = fromIntegral <$> [n .&. 0xff, (n .&. 0xff00) `shiftR` 16, (n .&. 0xff0000) `shiftR` 24, (n .&. 0xff000000) `shiftR` 32]

findLabelsAddresses :: [InterOpcode] -> Map Text Integer
findLabelsAddresses = findLabelsAddressesInternal 0
  where
    findLabelsAddressesInternal :: Integer -> [InterOpcode] -> Map Text Integer
    findLabelsAddressesInternal n []            = mempty
    findLabelsAddressesInternal n (Byte _:os)   = findLabelsAddressesInternal (n + 1) os
    findLabelsAddressesInternal n (Label l:os)  = Map.insert l n (findLabelsAddressesInternal (n + 4) os)
    findLabelsAddressesInternal n (Jump l:os)   = findLabelsAddressesInternal (n + 4) os
