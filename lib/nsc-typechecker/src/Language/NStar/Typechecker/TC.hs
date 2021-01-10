{-# LANGUAGE RecordWildCards #-}

module Language.NStar.Typechecker.TC where

import Control.Monad.State (StateT, modify)
import Control.Monad.Writer (WriterT)
import Control.Monad.Except (Except)
import Data.Map (Map)
import qualified Data.Map as Map
import Language.NStar.Typechecker.Env (Env)
import qualified Language.NStar.Typechecker.Env as Env
import Data.Located (Located)
import Data.Text (Text)
import Language.NStar.Typechecker.Core
import Data.Bifunctor (first, second)
import Language.NStar.Typechecker.Errors (TypecheckError, TypecheckWarning)

type Typechecker a = StateT (Integer, Context) (WriterT [TypecheckWarning] (Except TypecheckError)) a

-- | The data type of contexts in typechecking.
data Context
  = Ctx
  { typeEnvironment    :: Env Type                               -- ^ An 'Env'ironment containing labels associated to their expected contexts
  , currentTypeContext :: Map (Located Register) (Located Type)  -- ^ The current typechecking context
  , currentKindContext :: Map (Located Text) (Located Kind)      -- ^ The current kindchecking context
  , currentLabel       :: Maybe (Located Text)                   -- ^ The last crossed label
  , dataSections       :: Map (Located Text) (Located Type)      -- ^ The available labels in the @data@ sections
  }

instance Semigroup Context where
  Ctx te1 ctc1 ckc1 cl1 ds1 <> Ctx te2 ctc2 ckc2 cl2 ds2 =
    Ctx (te1 <> te2) (ctc1 <> ctc2) (ckc1 <> ckc2) currentLabel (ds1 <> ds2)
    where currentLabel = case (cl1, cl2) of
            (Nothing, Nothing) -> Nothing
            (Just l, _)        -> Just l
            (_, Just l)        -> Just l

instance Monoid Context where
  mempty = Ctx mempty mempty mempty Nothing mempty

-- | Adds a type to the environment.
addType :: Located Text -> Located Type -> Typechecker ()
addType k v = modify $ second modifyTypeContext
  where modifyTypeContext ctx@Ctx{..} = ctx { typeEnvironment = Env.insert k v typeEnvironment }

addDataLabel :: Located Text -> Located Type -> Typechecker ()
addDataLabel k v = modify $ second modifyDataSections
  where modifyDataSections ctx@Ctx{..} = ctx { dataSections = Map.insert k v dataSections }

-- | Increments the counter in the 'State' by one, effectively simulating a @counter++@ operation.
incrementCounter :: Typechecker ()
incrementCounter = modify $ first (+ 1)

setCurrentTypeContext :: Map (Located Register) (Located Type) -> Typechecker ()
setCurrentTypeContext newCtx = modify $ second putContext
  where putContext ctx = ctx { currentTypeContext = newCtx }

setCurrentKindContext :: Map (Located Text) (Located Kind) -> Typechecker ()
setCurrentKindContext newCtx = modify $ second putContext
  where putContext ctx = ctx { currentKindContext = newCtx }

setTypeEnvironment :: Env Type -> Typechecker ()
setTypeEnvironment newEnv = modify $ second setTypeEnv
  where setTypeEnv ctx = ctx { typeEnvironment = newEnv }

setLabel :: Located Text -> Typechecker ()
setLabel n = modify $ second setLbl
  where setLbl ctx = ctx { currentLabel = Just n }

--------------------------------------------------------
