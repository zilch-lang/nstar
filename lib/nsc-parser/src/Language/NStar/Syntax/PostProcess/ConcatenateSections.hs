module Language.NStar.Syntax.PostProcess.ConcatenateSections
( concatSections
) where

import Language.NStar.Syntax.Core
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Located (Position(..), Located((:@)))
import Data.Maybe (fromMaybe)

concatSections :: Program -> Program
concatSections (Program sects) = Program concatSects
  where
    concatSects = ($ groupSectionsByName sects) <$> [ dataSection, rodataSection, udataSection, codeSection ]
    {-# INLINE concatSects #-}

    dataSection = fromMaybe (Data [] :@ dummyPos) . Map.lookup "data"
    rodataSection = fromMaybe (ROData [] :@ dummyPos) . Map.lookup "rodata"
    udataSection = fromMaybe (UData [] :@ dummyPos) . Map.lookup "udata"
    codeSection = fromMaybe (Code [] :@ dummyPos) . Map.lookup "code"

    dummyPos = Position (1, 1) (1, 1) ""

groupSectionsByName :: [Located Section] -> Map String (Located Section)
groupSectionsByName = foldl (\acc e -> Map.insertWith concat' (sectionName e) e acc) mempty

sectionName :: Located Section -> String
sectionName (Data _ :@ _)   = "data"
sectionName (ROData _ :@ _) = "rodata"
sectionName (UData _ :@ _)  = "udata"
sectionName (Code _ :@ _)   = "code"

concat' :: Located Section -> Located Section -> Located Section
concat' (Data d1 :@ p) (Data d2 :@ _)     = Data (d1 <> d2) :@ p
concat' (ROData d1 :@ p) (ROData d2 :@ _) = ROData (d1 <> d2) :@ p
concat' (UData d1 :@ p) (UData d2 :@ _)   = UData (d1 <> d2) :@ p
concat' (Code c1 :@ p) (Code c2 :@ _)     = Code (c1 <> c2) :@ p
concat' s1 s2                             = error $ "Cannot concatenate sections of different types: " <> show s1 <> " and " <> show s2
