{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-orphans #-}

{-|
  Module: Language.NStar.Syntax.Internal
  Copyright: (c) Mesabloo, 2020
  License: BSD3
  Stability: experimental
-}

module Language.NStar.Syntax.Internal
( located
, showToken
) where

import Data.Located
import qualified Text.Megaparsec as MP
import Data.Bifunctor (second)
import qualified Data.Set as Set (toList)
import Language.NStar.Syntax.Core as Core (LToken, Token(..))
import qualified Data.List.NonEmpty as NonEmpty (toList)
import qualified Data.Text as Text (unpack)
import Data.List (intercalate)

-- | Wraps the result of a parser with its starting and ending positions.
located :: (MP.MonadParsec e s m, MP.TraversableStream s) => m a -> m (Located a)
located p = do
  MP.SourcePos
    { MP.sourceName   = file
    , MP.sourceLine   = lineB
    , MP.sourceColumn = colB } <- MP.getSourcePos
  let !start = both (fromIntegral . MP.unPos) (lineB, colB)

  res <- p

  MP.SourcePos
    { MP.sourceName   = _file
    , MP.sourceLine   = lineE
    , MP.sourceColumn = colE } <- MP.getSourcePos
  let !end = both (fromIntegral . MP.unPos) (lineE, colE)

  pure (res :@ Position start end file)

-- | Applies a computation to both element of a tuple.
--
--   > both f = bimap @(,) f f
both :: (a -> b) -> (a, a) -> (b, b)
both f ~(x, y) = (f x, f y)

----------------------------------------------------------------------------------------------------

-- instance {-# OVERLAPPING #-} MP.Stream [LToken] where
--   type Token [LToken] = LToken
--   type Tokens [LToken] = [LToken]

--   tokenToChunk _ = pure

--   tokensToChunk _ = id

--   chunkToTokens _ = id

--   chunkLength _ = length

--   chunkEmpty _ = null

--   take1_ []       = Nothing
--   take1_ (t : ts) = Just (t, ts)

--   takeN_ n s
--     | n <= 0    = Just ([], s)
--     | null s    = Nothing
--     | otherwise = Just (splitAt n s)

--   takeWhile_ = span

instance MP.VisualStream [LToken] where
  showTokens _ = commaSeparated . fmap (showToken . unLoc) . NonEmpty.toList

instance MP.TraversableStream [LToken] where
  reachOffset o MP.PosState{..} =
    let (before, after) = splitAt (o - pstateOffset) pstateInput

        actualisePos pos Nothing                               = pos
        actualisePos _ (Just (Position (bLine, bCol) _ file))  = MP.SourcePos
            { MP.sourceName   = file
            , MP.sourceColumn = MP.mkPos (fromIntegral bCol)
            , MP.sourceLine   = MP.mkPos (fromIntegral bLine)
            }

        tokenPos = case after of
            []           -> Nothing
            (_ :@ p) : _ -> Just p

        newPos = MP.PosState
            { MP.pstateInput      = after
            , MP.pstateOffset     = max pstateOffset o
            , MP.pstateSourcePos  = actualisePos pstateSourcePos tokenPos
            , MP.pstateTabWidth   = pstateTabWidth
            , MP.pstateLinePrefix = pstateLinePrefix}

        notEOL (t :@ _) = t /= Core.EOL

        fetchedLine = show $ reverse (takeWhile notEOL (reverse before)) <> takeWhile notEOL after
    in (Just fetchedLine, newPos)


commaSeparated :: [String] -> String
commaSeparated l = intercalate ", " $ filter (/= "") l

-- | A prettier output for 'Token's than its 'Show' instance.
showToken :: Token -> String
showToken (Integer i)          = "'" <> Text.unpack i <> "'"
showToken (Char c)             = "''" <> show c <> "''"
showToken (Id i)               = "'" <> Text.unpack i <> "'"
showToken (Str s)              = "'\"" <> Text.unpack s <> "\"'"
showToken R0'                  = "'r0'"
showToken R1'                  = "'r1'"
showToken R2'                  = "'r2'"
showToken R3'                  = "'r3'"
showToken R4'                  = "'r4'"
showToken R5'                  = "'r5'"
showToken Mv                   = "'mv'"
showToken Ret                  = "'ret'"
showToken Jmp                  = "'jmp'"
showToken Call                 = "'call'"
showToken Nop                  = "'nop'"
showToken Salloc               = "'salloc'"
showToken Sfree                = "'sfree'"
showToken Sld                  = "'sld'"
showToken Sst                  = "'sst'"
showToken Ld                   = "'ld'"
showToken St                   = "'st'"
showToken Sref                 = "'sref'"
showToken LParen               = "'('"
showToken LBrace               = "'{'"
showToken LBracket             = "'['"
showToken LAngle               = "'<'"
showToken RParen               = "')'"
showToken RBrace               = "'}'"
showToken RBracket             = "']'"
showToken RAngle               = "'>'"
showToken Star                 = "'*'"
showToken Dollar               = "'$'"
showToken Percent              = "'%'"
showToken Comma                = "','"
showToken Colon                = "':'"
showToken Dot                  = "'.'"
showToken Minus                = "'-'"
showToken Plus                 = "'+'"
showToken Equal                = "'='"
showToken Arrow                = "'->'"
showToken DoubleColon          = "'::'"
showToken Pipe                 = "'|'"
showToken Semi                 = "';'"
showToken Bang                 = "'!'"
showToken Forall               = "'forall'"
showToken TsK                  = "'Ts'"
showToken TcK                  = "'Tc'"
showToken TaK                  = "'Ta'"
showToken (TnK n)              = "'T" <> show n <> "'"
showToken (InlineComment _)    = ""
showToken (MultilineComment _) = ""
showToken EOL                  = "<eol>"
showToken EOF                  = "<eof>"
showToken HSpace               = "<space>"
showToken UnSafe               = "'unsafe'"
showToken Section              = "'section'"
showToken Include              = "'include'"
