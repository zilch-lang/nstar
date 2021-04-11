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
    concatSects = ($ groupSectionsByName sects) <$> [ dataSection, rodataSection, udataSection, codeSection, externCodeSection ]
    {-# INLINE concatSects #-}

    dataSection = fromMaybe (DataS [] :@ dummyPos) . Map.lookup "data"
    rodataSection = fromMaybe (RODataS [] :@ dummyPos) . Map.lookup "rodata"
    udataSection = fromMaybe (UDataS [] :@ dummyPos) . Map.lookup "udata"
    codeSection = fromMaybe (CodeS [] :@ dummyPos) . Map.lookup "code"
    externCodeSection = fromMaybe (ExternCodeS [] :@ dummyPos) . Map.lookup "extern.code"

    dummyPos = Position (1, 1) (1, 1) ""

groupSectionsByName :: [Located Section] -> Map String (Located Section)
groupSectionsByName = foldl (\acc e -> Map.insertWith (flip concat') (sectionName e) e acc) mempty

sectionName :: Located Section -> String
sectionName (DataS _ :@ _)       = "data"
sectionName (RODataS _ :@ _)     = "rodata"
sectionName (UDataS _ :@ _)      = "udata"
sectionName (CodeS _ :@ _)       = "code"
sectionName (IncludeS _ :@ _)    = "include"
sectionName (ExternCodeS _ :@ _) = "extern.code"

concat' :: Located Section -> Located Section -> Located Section
concat' (DataS d1 :@ p) (DataS d2 :@ _)             = DataS (d1 <> d2) :@ p
concat' (RODataS d1 :@ p) (RODataS d2 :@ _)         = RODataS (d1 <> d2) :@ p
concat' (UDataS d1 :@ p) (UDataS d2 :@ _)           = UDataS (d1 <> d2) :@ p
concat' (CodeS c1 :@ p) (CodeS c2 :@ _)             = CodeS (c1 <> c2) :@ p
concat' (ExternCodeS e1 :@ p) (ExternCodeS e2 :@ _) = ExternCodeS (e1 <> e2) :@ p
concat' s1 s2                             = error $ "Cannot concatenate sections of different types: " <> show s1 <> " and " <> show s2
