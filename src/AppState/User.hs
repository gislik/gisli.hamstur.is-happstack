{-# LANGUAGE FlexibleContexts #-}
module AppState.User where

import Prelude hiding (lookup)
import Control.Monad.Reader
import Data.Digest.OpenSSL.MD5 (md5sum)
import Data.ByteString.Char8 (pack)
import Data.Map
import AppState.Types
import Model.User

askUsers :: (MonadReader AppState m) => m Users
askUsers = return . users =<< ask
 

listUsers :: (MonadReader AppState m) => m [String]
listUsers = liftM keys askUsers

authUser :: (MonadReader AppState m) => String -> String -> m (Maybe User)
authUser name pass = do
         user <- liftM (lookup name) askUsers
         case user of
              Just user' -> do
                   if password user' == (Just . md5sum.pack) pass
                      then return user
                      else return Nothing
              Nothing -> return Nothing

getUser :: (MonadReader AppState m) => String -> m (Maybe User)
getUser name = liftM (lookup name) askUsers
