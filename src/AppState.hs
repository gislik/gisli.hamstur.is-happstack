{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, TypeFamilies, 
             FlexibleContexts, FlexibleInstances,
             MultiParamTypeClasses, TypeSynonymInstances #-}
module AppState where

import Happstack.Server
import Happstack.State
import Control.Monad.State
import Control.Monad.Reader
import Data.Char
import Data.Map
import Data.Maybe
import System.Time (ClockTime(TOD))
import Model.Blog
import Model.Tag
import Model.User
import AppEnv
import AppState.Types
import AppState.Session
import AppState.Blog
import AppState.Tag
import AppState.User
import AppState.AppDate


instance Component AppState where 
         type Dependencies AppState = End 
         initialValue = AppState { 
                      blogs =  [],
                      blogsLastUpdated = NoAppDate,
                      tags = singleton "happs" (Tag { tagBlogs=[1,3], tagSeries=False, tagCount=2 }),
                      users = singleton username user,
                      sessions = Session { session=empty }
                      }
                      where user :: User
                            user = User username password name
                            username = "gislik"
                            password = Just "e47bb6e298348b2fa1aaa5e7c8c40cb1"
                            name = "G&iacute;sli Kristj&aacute;nsson"

$(mkMethods ''AppState ['listBlogs, 'listAllBlogs, 'getBlog, 'getBlog', 'modBlogs, 'addBlog, 'deleteBlog, 'listTags, 'getTag, 'authUser, 'getUser, 'getSession, 'isSession, 'setSession, 'newSession, 'delSession, 'cleanSessions, 'listSessions, 'listSessionData, 'modBlogsLastUpdated, 'getBlogsLastUpdated])
