{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ApplicativeDo #-}

module Console.NStar.Flags
( extractFlags
,  -- * Re-exports
  Flags(..), ConfigurationFlags(..)
, LexerFlags(..), ParserFlags(..), TypecheckerFlags(..)
) where

import Console.NStar.Flags.Internal
import Options.Applicative
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as MegaC
import Data.Data (Data)
import Data.Typeable (Typeable)
import Data.Char (toLower)
import qualified Data.Set as Set
import Data.Foldable (fold)
import Data.Bifunctor (first)
import Control.Monad (join)
import Data.List (intercalate)

extractFlags :: IO Flags
extractFlags = customExecParser preferences opts
  where
    opts = info (cli <**> helper) (fullDesc)
    preferences = prefs showHelpOnError

cli :: Parser Flags
cli = do
  f <- Flags
    <$> many (argument str (metavar "FILES..."))
    <*> (mconcat <$> many config)
  pure f

config :: Parser ConfigurationFlags
config = do
  fl <- option (eitherReader parseConfigFlag)
               (short 'f' <> metavar "OPTION[=VALUE]" <> hidden <> help "See available configuration OPTIONs at <https://github.com/zilch-lang/nsc/blob/develop/docs/compiler-options.md>" <> completeWith configKeys)
  pure $ mempty
    { diagnostic_color = maybe True fromYesNo (join $ Map.lookup "color-diagnostics" fl) }
  where
    configKeys =
      [ "color-diagnostics" ]

configOptions :: Parser ()
configOptions = subparser $ commandGroup "Available configuration (option -f):" <> hidden <> fold
  [ command "color-diagnostics=<yes|no>" (noop $ progDesc "Whether to enable colored errors/messages" <> footer "Defaults to 'no' if unspecified")
  ]
  where
    noop = info (option (readerError "This should never be printed!") idm)




----------------------------------------------------------------------------------------------------

fromYesNo :: String -> Bool
fromYesNo "yes" = True
fromYesNo "no"  = False

parseConfigFlag :: String -> Either String (Map String (Maybe String))
parseConfigFlag = first toStringError . Mega.runParser configFlags "cli-config"
  where
    toStringError Mega.ParseErrorBundle{..} =
      fold (Mega.parseErrorTextPretty <$> bundleErrors)

data ErrorContext
  = ErrCtx
  { inFields :: String
  , err      :: Mega.ParseError String ErrorContext
  }
  deriving (Eq, Show, Typeable, Data)
instance Ord ErrorContext where
  ErrCtx _ e1 <= ErrCtx _ e2 = off e1 <= off e2
    where
      off (Mega.TrivialError o _ _) = o
      off (Mega.FancyError o _)     = o

instance Mega.ShowErrorComponent ErrorContext where
  showErrorComponent ErrCtx{..} =
    fold (drop 1 (lines (Mega.parseErrorTextPretty err))) <> " in configuration definition for key '" <> inFields <> "'"

configFlags :: Mega.Parsec ErrorContext String (Map String (Maybe String))
configFlags = uncurry Map.singleton <$> Mega.choice
  [ field "color-diagnostics" yesno, unknownField ]

field :: String -> Mega.Parsec ErrorContext String String -> Mega.Parsec ErrorContext String (String, Maybe String)
field name val = Mega.region (\ p@(Mega.TrivialError o _ _) -> Mega.FancyError o . Set.singleton . Mega.ErrorCustom $ ErrCtx name p) $
  (,) <$> MegaC.string name
      <*> Mega.optional (MegaC.char '=' *> val)

unknownField :: Mega.Parsec ErrorContext String (String, Maybe String)
unknownField = fail . (\ k -> "Unknown configuration key '" <> k <> "'") =<< Mega.takeWhileP Nothing (/= '=')

yesno :: Mega.Parsec ErrorContext String String
yesno = fmap toLower <$> Mega.choice [ MegaC.string' "yes", MegaC.string' "no" ]
