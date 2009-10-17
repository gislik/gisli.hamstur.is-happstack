{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, TypeFamilies, 
             FlexibleContexts, FlexibleInstances,
             MultiParamTypeClasses, TypeSynonymInstances #-}


-- move this to Model.App (App.Model) - I think it makes more sense
module AppState.Types where

import Happstack.Server
import Happstack.State
import Data.Generics
import Control.Monad.Reader
import Control.Monad.State
import Data.Map
import qualified Data.Map as M
import GHC.Conc
import Model.Blog
import Model.Tag
import Model.User
import Text.StringTemplate (ToSElem, toSElem)

type SessionID = String

data Session a = Session {
     session :: Map SessionID a
} deriving (Show, Read, Eq, Data, Typeable)

instance Version (Session a)
$(deriveSerialize ''Session)

data SessionData = SessionData {
     user :: User
} deriving (Show, Read, Eq, Data, Typeable)


instance ToSElem SessionData where
 	 toSElem sdata = (toSElem.fromList) [("user", user sdata)]

instance Version SessionData
$(deriveSerialize ''SessionData)


data AppDate = AppDate Integer Integer | NoAppDate
         deriving (Show, Read, Data, Typeable, Eq)

instance Version AppDate
$(deriveSerialize ''AppDate)

data AppState = AppState {
     blogs            :: Blogs,
     blogsLastUpdated :: AppDate,
     tags             :: Tags,
     users            :: Users,
     sessions         :: Session SessionData
} deriving (Show, Read, Typeable, Data)                                         

instance Version AppState
$(deriveSerialize ''AppState) 


