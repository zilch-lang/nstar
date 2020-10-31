{-# LANGUAGE TypeApplications #-}

module Data.Elf.Internal.ToBytes (ToBytes(..)) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS (pack)
import Data.Word (Word8, Word16, Word32, Word64)
import Data.Int (Int32, Int64)
import Data.Bits ((.&.), shiftR)

class ToBytes a where
  -- | Transforms some sort of data into its binary representation (for easier binary file writing).
  toBytes
    :: Bool        -- ^ little endian (@True@) or big endian (@False@)
    -> a           -- ^ Data to be encoded into its binary representation
    -> ByteString

instance ToBytes a => ToBytes [a] where
  toBytes le = mconcat . fmap (toBytes le)

instance ToBytes Word8 where
  toBytes _ = BS.pack . pure

instance ToBytes Word16 where
  toBytes True  = BS.pack . encodeWord16LE
  toBytes False = BS.pack . encodeWord16BE

encodeWord16LE, encodeWord16BE :: Word16 -> [Word8]
encodeWord16LE w = [fromIntegral (w .&. 0xff), fromIntegral ((w .&. 0xff00) `shiftR` 8)]
encodeWord16BE = reverse . encodeWord16LE

instance ToBytes Word32 where
  toBytes True  = BS.pack . encodeWord32LE
  toBytes False = BS.pack . encodeWord32BE

encodeWord32LE, encodeWord32BE :: Word32 -> [Word8]
encodeWord32LE w = encodeWord16LE (fromIntegral (w .&. 0xffff)) <> encodeWord16LE (fromIntegral ((w .&. 0xffff0000) `shiftR` 16))
                -- [w .&. 0xff, (w .&. 0xff00) `shiftR` 8, (w .&. 0xff0000) `shiftR` 16, (w .&. 0xff000000) `shiftR` 24]
encodeWord32BE = reverse . encodeWord32LE

instance ToBytes Word64 where
  toBytes True  = BS.pack . encodeWord64LE
  toBytes False = BS.pack . encodeWord64BE

encodeWord64LE, encodeWord64BE :: Word64 -> [Word8]
encodeWord64LE w = encodeWord32LE (fromIntegral (w .&. 0xffffffff)) <> encodeWord32LE (fromIntegral ((w .&. 0xffffffff00000000) `shiftR` 32))
encodeWord64BE = reverse . encodeWord64LE

instance ToBytes Int32 where
  toBytes le = toBytes @Word32 le . fromIntegral

instance ToBytes Int64 where
  toBytes le = toBytes @Word64 le . fromIntegral
