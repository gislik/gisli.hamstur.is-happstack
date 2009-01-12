{-# LANGUAGE FlexibleContexts #-}
module AppState.Tag where

--import Control.Monad.State (MonadState, modify)
import Control.Monad.Reader
import Control.Monad
import AppState.Types
import Prelude
import qualified Prelude as P
import Data.Char (toLower)
--import AppState.AppDate
import Data.Map
import qualified Data.Map as M
import Model.Tag


listTags :: MonadReader AppState m => m [String]
listTags = return.keys.tags =<< ask

getTag :: MonadReader AppState m => String -> m (Maybe Tag)
getTag tag = liftM (M.lookup (P.map toLower tag).tags) ask