{-# LANGUAGE FlexibleContexts #-}

module AppState.Session where

import Prelude hiding (lookup)
import HAppS.Server
import HAppS.State
import GHC.Conc
import Control.Monad.Reader
import Control.Monad.State
import Data.Map
import Data.Digest.OpenSSL.MD5 (md5sum)
import Data.ByteString.Char8 (pack)
import AppState.Types
import Model.User

askSession :: (MonadReader AppState m) => m (Session SessionData)
askSession = return . sessions =<< ask

modSession :: (MonadState AppState m) => (Session SessionData -> Session SessionData) -> m ()
modSession f = modify $ \state@(AppState {sessions=s}) -> state {sessions=f s}

isSession :: (MonadReader AppState m) => SessionID -> m Bool
isSession sid = liftM (member sid . session) askSession

getSession :: (MonadReader AppState m) => SessionID -> m (Maybe SessionData)
getSession sid = liftM (lookup sid . session) askSession

setSession :: (MonadState AppState m) => SessionID -> SessionData -> m ()
setSession sid sdata = modSession $ Session . insert sid sdata . session 

newSession :: (MonadState
                 AppState (Ev (t STM)),
               Monad (t STM),
               MonadTrans t) =>
              SessionData -> Ev (t STM) SessionID
newSession sdata = do
           sid <- getRandom
           let ssid = ((md5sum.pack.show) (sid::Integer))::SessionID
           setSession ssid sdata
           return ssid

listSessions :: (MonadReader AppState m) => m [SessionID]
listSessions = liftM (keys . session) askSession

listSessionData :: (MonadReader AppState m) => m [(SessionID, SessionData)]
listSessionData = liftM (assocs . session) askSession

delSession :: (MonadState AppState m) => SessionID -> m ()
delSession sid = modSession $ Session . (delete sid) . session

cleanSessions :: (MonadState AppState m) => SessionID -> m()
cleanSessions sid = modSession $ Session . maybe empty (singleton sid) . lookup sid . session