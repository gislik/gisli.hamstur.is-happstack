{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, TypeFamilies,
             TypeSynonymInstances #-} 

module Model.User where

import HAppS.State
import Data.Generics
import Data.Map
import Text.StringTemplate

data User = NoUser | User {
     username :: String,
     password :: Maybe String,
     name     :: String
} deriving (Show, Read, Eq, Data, Typeable)

instance Version User
$(deriveSerialize ''User)

type Users = Map String User

instance ToSElem User where
         toSElem NoUser = toSElem "***"
 	 toSElem u      = (toSElem.fromList) [("username", username u), 
                                              ("password", (maybe "***" id.password) u), 
                                              ("name", name u)]