{-|
  Module: Language.NStar.Syntax
  Copyright: (c) Mesabloo, 2020
  License: BSD3
  Stability: experimental
-}
module Language.NStar.CodeGen
( -- * Re-exports
  module Language.NStar.CodeGen.Arch
, module Language.NStar.CodeGen.Elf
, module Data.Elf
) where

import Language.NStar.CodeGen.Arch
import Language.NStar.CodeGen.Elf
import Data.Elf
