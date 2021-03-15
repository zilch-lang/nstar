module Language.NStar.CodeGen.Machine.X64.Expression where

import Language.NStar.Syntax.Core (Expr(..), Constant(..), Immediate(..))
import Data.Word (Word8)
import Data.Located (Located((:@)), unLoc)
import qualified Data.ByteString.Lazy as BS
import Language.NStar.CodeGen.Compiler (Compiler)
import Language.NStar.CodeGen.Machine.Internal.Intermediate (InterOpcode(..))
import Data.Binary.Put (runPut, putInt64le, putInt32le, putInt8, putWord8)
import Data.Char (ord)

compileExprX64 :: Integer -> Expr -> Compiler [InterOpcode]
compileExprX64 n (ImmE i) = case (n, unLoc i) of
  (32, I int) -> pure (Byte <$> int32 int)
  (64, I int) -> pure (Byte <$> int64 int)
  (64, C c)   -> pure (Byte <$> char8 c)
-- ^^^^^ all these matches are intentionally left incomplete.

int64 :: Integer -> [Word8]
int64 = BS.unpack . runPut . putInt64le . fromIntegral

int32 :: Integer -> [Word8]
int32 = BS.unpack . runPut . putInt32le . fromIntegral

int8 :: Integer -> [Word8]
int8 = BS.unpack . runPut . putInt8 . fromIntegral

char8 :: Char -> [Word8]
char8 = BS.unpack . runPut . putWord8 . fromIntegral . ord

compileConstantX64 :: Constant -> [Word8]
compileConstantX64 (IntegerC (i :@ _))   = int64 i
compileConstantX64 (CharacterC (c :@ _)) = char8 c
compileConstantX64 (ArrayC csts)         = mconcat (compileConstantX64 . unLoc <$> csts)
