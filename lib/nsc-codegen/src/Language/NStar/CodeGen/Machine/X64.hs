module Language.NStar.CodeGen.Machine.X64 (compileX64) where

import Language.NStar.CodeGen.Compiler
import Language.NStar.Typechecker.Core
import Language.NStar.CodeGen.Machine.Internal.Intermediate (InterOpcode(..))
import Data.Located (unLoc, Located(..))
import Language.NStar.CodeGen.Errors

compileX64 :: TypedProgram -> Compiler ()
compileX64 = (fixupAddressesX64 =<<) . compileInterX64

compileInterX64 :: TypedProgram -> Compiler [InterOpcode]
compileInterX64 (TProgram stmts) = (<>) <$> compileStmtInterX64 . unLoc <$> stmts

compileInterStmtInterX64 :: TypedStatement -> Compiler [InterOpcode]
compileInterStmtInterX64 (TLabel name) = pure [Label name]
compileInterStmtInterX64 (TInstr i ts) = compileInterInstrX64 (unLoc i) (unLoc <$> ts)

compileInterInstrX64 :: Instruction -> [Type] -> Compiler [InterOpcode]
compileInterInstrX64 Ret [] = pure [Byte 0xC3]
compileInterInstrX64 Ret args = throwError (incorrectArgsTypesForInstr Ret args)
compileInterInstrX64 i args = error $ "not yet implemented: compileInterInstrX64 " <> show i <> " " <> show args

fixupAddressesX64 :: [InterOpcode] -> Compile ()
fixupAddressesX64 _ = pure ()
