{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.NStar.Syntax.Internal
( located
, megaparsecBundleToDiagnostic
) where

import Data.Located
import qualified Text.Megaparsec as MP
import Text.Diagnose (Diagnostic, diagnostic, reportError, Marker(This, Where), (<++>), Hint, Report)
import Data.Bifunctor (second)
import Language.NStar.Syntax.Hints
import qualified Data.Set as Set (toList)

-- | Wraps the result of a parser with its starting and ending positions.
located :: (MP.MonadParsec e s m) => m a -> m (Located a)
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

-- | Transforms a megaparsec's 'MP.ParseErrorBundle' into a well formated 'Diagnostic'.
megaparsecBundleToDiagnostic :: (MP.Stream s, Hintable e String, MP.ShowErrorComponent e) => String -> MP.ParseErrorBundle s e -> Diagnostic s2 String a
megaparsecBundleToDiagnostic msg MP.ParseErrorBundle{..} =
  foldl (<++>) diagnostic (toLabeledPositions <$> bundleErrors)
 where toLabeledPositions :: (MP.Stream s, Hintable e String, MP.ShowErrorComponent e) => MP.ParseError s e -> Report String
       toLabeledPositions err =
         let (_, pos) = MP.reachOffset (MP.errorOffset err) bundlePosState
             !source  = fromSourcePos (MP.pstateSourcePos pos)
             msgs     = lines (MP.parseErrorTextPretty err)
         in flip (reportError msg) (errorHints err)
            if | [m] <- msgs      -> [ (source, This m) ]
               | [m1, m2] <- msgs -> [ (source, This m1), (source, Where m2) ]
               | otherwise        -> [ (source, This "Unknown error") ]

       fromSourcePos MP.SourcePos{..} =
         let start = both (fromIntegral . MP.unPos) (sourceLine, sourceColumn)
             end   = second (+ 1) start
         in Position start end sourceName

       errorHints :: (MP.Stream s, Hintable e String) => MP.ParseError s e -> [Hint String]
       errorHints MP.TrivialError{}      = []
       errorHints (MP.FancyError _ errs) = Set.toList errs >>= \case
         MP.ErrorCustom e -> hints e
         _                -> mempty
