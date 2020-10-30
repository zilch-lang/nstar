module Language.NStar.CodeGen.Machine.X64 (compileX64) where

import Language.NStar.CodeGen.Compiler
import Language.NStar.Typechecker.Core
import Language.NStar.CodeGen.Machine.Internal.Intermediate (InterOpcode(..))
import Data.Located (unLoc, Located(..))
import Language.NStar.CodeGen.Errors
import Control.Monad.Except (throwError)
import Internal.Error (internalError)

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
fixupAddressesX64 _ = pure ()
