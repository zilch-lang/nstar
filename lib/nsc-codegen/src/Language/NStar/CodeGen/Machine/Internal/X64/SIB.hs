{-# LANGUAGE BinaryLiterals #-}

module Language.NStar.CodeGen.Machine.Internal.X64.SIB
( -- * Calculating the SIB byte
  -- $sib
  -- * SIB constructors
sib, sibDisp32, sibRegDisp
) where

import Language.NStar.Syntax.Core (Register)
import Data.Word (Word8)
import Data.Bits (shiftL, (.&.), (.|.))
import Language.NStar.CodeGen.Machine.Internal.Intermediate (InterOpcode(..))
import Language.NStar.CodeGen.Machine.Internal.X64.RegisterEncoding (registerNumber)

{- $sib

Table 2-3 is organized to give 256 possible values of the SIB byte (in hexadecimal).
General purpose registers used as a base are indicated across the top of the table, along with corresponding values for the SIB byte’s base field.
Table rows in the body of the table indicate the register used as the index (SIB byte bits 3, 4 and 5) and the scaling factor (determined by SIB byte bits 6 and 7).

+----------------------------+-----+-----+-----+-----+-----+-----+-----+-----+
|             r32            | EAX | ECX | EDX | EBX | ESP | [*] | ESI | EDI |
+----------------------------+-----+-----+-----+-----+-----+-----+-----+-----+
|     (In decimal) Base =    |  0  |  1  |  2  |  3  |  4  |  5  |  6  |  7  |
+----------------------------+-----+-----+-----+-----+-----+-----+-----+-----+
|     (In binary) Base =     | 000 | 001 | 010 | 011 | 100 | 101 | 110 | 111 |
+--------------+-----+-------+-----+-----+-----+-----+-----+-----+-----+-----+
| Scaled Index | SS  | Index |       Value of SIB Byte (in Hexadecimal)      |
+==============+=====+=======+=====+=====+=====+=====+=====+=====+=====+=====+
|     [EAX]    |  00 |  000  |  00 |  01 |  02 |  03 |  04 |  05 |  06 |  07 |
+--------------+     +-------+-----+-----+-----+-----+-----+-----+-----+-----+
|     [ECX]    |     |  001  |  08 |  09 |  0A |  0B |  0C |  0D |  0E |  0F |
+--------------+     +-------+-----+-----+-----+-----+-----+-----+-----+-----+
|     [EDX]    |     |  010  |  10 |  11 |  12 |  13 |  14 |  15 |  16 |  17 |
+--------------+     +-------+-----+-----+-----+-----+-----+-----+-----+-----+
|     [EBX]    |     |  011  |  18 |  19 |  1A |  1B |  1C |  1D |  1E |  1F |
+--------------+     +-------+-----+-----+-----+-----+-----+-----+-----+-----+
|     none     |     |  100  |  20 |  21 |  22 |  23 |  24 |  25 |  26 |  27 |
+--------------+     +-------+-----+-----+-----+-----+-----+-----+-----+-----+
|     [EBP]    |     |  101  |  28 |  29 |  2A |  2B |  2C |  2D |  2E |  2F |
+--------------+     +-------+-----+-----+-----+-----+-----+-----+-----+-----+
|     [ESI]    |     |  110  |  30 |  31 |  32 |  33 |  34 |  35 |  36 |  37 |
+--------------+     +-------+-----+-----+-----+-----+-----+-----+-----+-----+
|     [EDI]    |     |  111  |  38 |  39 |  3A |  3B |  3C |  3D |  3E |  3F |
+--------------+-----+-------+-----+-----+-----+-----+-----+-----+-----+-----+
|   [EAX * 2]  |  01 |  000  |  40 |  41 |  42 |  43 |  44 |  45 |  46 |  47 |
+--------------+     +-------+-----+-----+-----+-----+-----+-----+-----+-----+
|   [ECX * 2]  |     |  001  |  48 |  49 |  4A |  4B |  4C |  4D |  4E |  4F |
+--------------+     +-------+-----+-----+-----+-----+-----+-----+-----+-----+
|   [EDX * 2]  |     |  010  |  50 |  51 |  52 |  53 |  54 |  55 |  56 |  57 |
+--------------+     +-------+-----+-----+-----+-----+-----+-----+-----+-----+
|   [EBX * 2]  |     |  011  |  58 |  59 |  5A |  5B |  5C |  5D |  5E |  5F |
+--------------+     +-------+-----+-----+-----+-----+-----+-----+-----+-----+
|     none     |     |  100  |  60 |  61 |  62 |  63 |  64 |  65 |  66 |  67 |
+--------------+     +-------+-----+-----+-----+-----+-----+-----+-----+-----+
|   [EBP * 2]  |     |  101  |  68 |  69 |  6A |  6B |  6C |  6D |  6E |  6F |
+--------------+     +-------+-----+-----+-----+-----+-----+-----+-----+-----+
|   [ESI * 2]  |     |  110  |  70 |  71 |  72 |  73 |  74 |  75 |  76 |  77 |
+--------------+     +-------+-----+-----+-----+-----+-----+-----+-----+-----+
|   [EDI * 2]  |     |  111  |  78 |  79 |  7A |  7B |  7C |  7D |  7E |  7F |
+--------------+-----+-------+-----+-----+-----+-----+-----+-----+-----+-----+
|   [EAX * 4]  |  10 |  000  |  80 |  81 |  82 |  83 |  84 |  85 |  86 |  87 |
+--------------+     +-------+-----+-----+-----+-----+-----+-----+-----+-----+
|   [ECX * 4]  |     |  001  |  88 |  89 |  8A |  8B |  8C |  8D |  8E |  8F |
+--------------+     +-------+-----+-----+-----+-----+-----+-----+-----+-----+
|   [EDX * 4]  |     |  010  |  90 |  91 |  92 |  93 |  94 |  95 |  96 |  97 |
+--------------+     +-------+-----+-----+-----+-----+-----+-----+-----+-----+
|   [EBX * 4]  |     |  011  |  98 |  99 |  9A |  9B |  9C |  9D |  9E |  9F |
+--------------+     +-------+-----+-----+-----+-----+-----+-----+-----+-----+
|     none     |     |  100  |  A0 |  A1 |  A2 |  A3 |  A4 |  A5 |  A6 |  A7 |
+--------------+     +-------+-----+-----+-----+-----+-----+-----+-----+-----+
|   [EBP * 4]  |     |  101  |  A8 |  A9 |  AA |  AB |  AC |  AD |  AE |  AF |
+--------------+     +-------+-----+-----+-----+-----+-----+-----+-----+-----+
|   [ESI * 4]  |     |  110  |  B0 |  B1 |  B2 |  B3 |  B4 |  B5 |  B6 |  B7 |
+--------------+     +-------+-----+-----+-----+-----+-----+-----+-----+-----+
|   [EDI * 4]  |     |  111  |  B8 |  B9 |  BA |  BB |  BC |  BD |  BE |  BF |
+--------------+-----+-------+-----+-----+-----+-----+-----+-----+-----+-----+
|   [EAX * 8]  |  11 |  000  |  C0 |  C1 |  C2 |  C3 |  C4 |  C5 |  C6 |  C7 |
+--------------+     +-------+-----+-----+-----+-----+-----+-----+-----+-----+
|   [ECX * 8]  |     |  001  |  C8 |  C9 |  CA |  CB |  CC |  CD |  CE |  CF |
+--------------+     +-------+-----+-----+-----+-----+-----+-----+-----+-----+
|   [EDX * 8]  |     |  010  |  D0 |  D1 |  D2 |  D3 |  D4 |  D5 |  D6 |  D7 |
+--------------+     +-------+-----+-----+-----+-----+-----+-----+-----+-----+
|   [EBX * 8]  |     |  011  |  D8 |  D9 |  DA |  DB |  DC |  DD |  DE |  DF |
+--------------+     +-------+-----+-----+-----+-----+-----+-----+-----+-----+
|     none     |     |  100  |  E0 |  E1 |  E2 |  E3 |  E4 |  E5 |  E6 |  E7 |
+--------------+     +-------+-----+-----+-----+-----+-----+-----+-----+-----+
|   [EBP * 8]  |     |  101  |  E8 |  E9 |  EA |  EB |  EC |  ED |  EE |  EF |
+--------------+     +-------+-----+-----+-----+-----+-----+-----+-----+-----+
|   [ESI * 8]  |     |  110  |  F0 |  F1 |  F2 |  F3 |  F4 |  F5 |  F6 |  F7 |
+--------------+     +-------+-----+-----+-----+-----+-----+-----+-----+-----+
|   [EDI * 8]  |     |  111  |  F8 |  F9 |  FA |  FB |  FC |  FD |  FE |  FF |
+--------------+-----+-------+-----+-----+-----+-----+-----+-----+-----+-----+

=== NOTES:
1. The @[*]@ nomenclature means a @disp32@ with no base if the MOD is @00B@. Otherwise, @[*]@ means @disp8@ or @disp32 + [EBP]@.

     This provides the following address modes:

     [MOD bits] Effective Address

     ==============================================

     [00] @[scaled index] + disp32@
     [01] @[scaled index] + disp8 + [EBP]@
     [10] @[scaled index] + disp32 + [EBP]@


===== Table generated from the Intel manual (Volume 2, Section 2.1) with the help of <https://www.tablesgenerator.com/text_tables>.
-}

-- | Creates the SIB byte from the scale, the index and the base.
--
--   >>> effective_address = scale * index + base + offset
sib :: Word8     -- ^ The scale
    -> Word8     -- ^ The index
    -> Word8     -- ^ The base
    -> InterOpcode
sib scale index base = Byte $
      ((scale .&. 0b11)  `shiftL` 6)
  .|. ((index .&. 0b111) `shiftL` 3)
  .|. ((base .&. 0b111)  `shiftL` 0)

-- | Creates a SIB byte indicating a 32 bit displacement from an address.
sibDisp32 :: InterOpcode
sibDisp32 = sib 0x0 0x4 0x5

-- | Creates a SIB byte indicating a displacement of the form @[base + index * scale]@ where @base@ and @index@ are registers.
sibRegDisp :: Register -> Register -> Word8 -> InterOpcode
sibRegDisp base index scale = sib scaleEnum (registerNumber index) (registerNumber base)
  where scaleEnum = case scale of
          1 -> 0b00
          2 -> 0b01
          4 -> 0b10
          8 -> 0b11
          _ -> error ("Invalid scale specifier " <> show scale)
