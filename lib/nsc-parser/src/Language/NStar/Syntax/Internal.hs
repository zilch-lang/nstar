{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-orphans #-}

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
import Language.NStar.Syntax.Core as Core (LToken, Token(..))
import qualified Data.List.NonEmpty as NonEmpty (toList)
import qualified Data.Text as Text (unpack)
import Data.List (intercalate)

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

----------------------------------------------------------------------------------------------------

instance MP.Stream [LToken] where
  type Token [LToken] = LToken
  type Tokens [LToken] = [LToken]

  tokenToChunk _ = pure

  tokensToChunk _ = id

  chunkToTokens _ = id

  chunkLength _ = length

  chunkEmpty _ = null

  take1_ []       = Nothing
  take1_ (t : ts) = Just (t, ts)

  takeN_ n s
    | n <= 0    = Just ([], s)
    | null s    = Nothing
    | otherwise = Just (splitAt n s)

  takeWhile_ = span

  showTokens _ = commaSeparated . fmap (showToken . unLocate) . NonEmpty.toList
    where unLocate (t :@ _) = t

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
    in (fetchedLine, newPos)

commaSeparated :: [String] -> String
commaSeparated l = intercalate ", " $ filter (/= "") l

showToken :: Token -> String
showToken (Integer i)          = "integer '" <> show i <> "'"
showToken (Char c)             = "character '" <> show c <> "'"
showToken (Id i)               = "identifier '" <> Text.unpack i <> "'"
showToken Rax                  = "register 'rax'"
showToken Rbx                  = "register 'rbx'"
showToken Rcx                  = "register 'rcx'"
showToken Rdx                  = "register 'rdx'"
showToken Rdi                  = "register 'rdi'"
showToken Rsi                  = "register 'rsi'"
showToken Rsp                  = "register 'rsp'"
showToken Rbp                  = "register 'rbp'"
showToken Mov                  = "instruction 'mov'"
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
showToken DoubleColon          = "'::'"
showToken Forall               = "'forall'"
showToken Sptr                 = "'sptr'"
showToken (InlineComment _)    = ""
showToken (MultilineComment _) = ""
showToken EOL                  = "<eol>"
showToken EOF                  = "<eof>"
