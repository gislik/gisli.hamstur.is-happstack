module Controller.Atom (atomController) where

import HAppS.Server
import HAppS.State
import Control.Monad.Trans
import Control.Monad
import Helper
import Util.Atom
import AppState
import AppState.Blog
import AppState.AppDate
import Model.Blog
import Model.User

atomController :: [ServerPartT IO Response]
atomController = [dirindex [anyRequest atom]]

atom :: WebT IO Response
atom = do
     lastUpdated <- liftM fromAppDate $ query GetBlogsLastUpdated
     (showFeed . toFeed lastUpdated) =<<  blogs
     where blogs = liftIO.liftM (reverse.sortByDate).query $ ListBlogs
