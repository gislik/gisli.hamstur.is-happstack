{-# LANGUAGE FlexibleContexts #-}
module AppState.Blog where

import Control.Monad.State (modify)
import Control.Monad.Reader
import Happstack.State
import Data.List
import AppState.Types
import AppState.AppDate
import Model.Blog

--listBlogs :: MonadReader AppState m => m [Blog]
listBlogs :: Query AppState [Blog]
listBlogs = liftM (filter (not.blogPrivate)) listAllBlogs

--listAllBlogs :: MonadReader AppState m => m [Blog]
listAllBlogs :: Query AppState [Blog]
listAllBlogs = return . blogs =<< ask

--modBlogs :: (MonadState AppState m) => Blogs -> m ()
modBlogs :: Blogs -> Update AppState ()
modBlogs bs = modify (\s -> s { blogs = bs })

--getBlog :: (MonadReader AppState m) => BlogID -> m (Maybe Blog)
getBlog :: BlogID -> Query AppState (Maybe Blog)
getBlog bid = return.find (\b -> blogID b == bid) =<< listBlogs

--getBlog' :: (MonadReader AppState m) => BlogID -> m (Maybe Blog)
getBlog' :: BlogID -> Query AppState (Maybe Blog)
getBlog' bid = return.find (\b -> blogID b == bid) =<< listAllBlogs
           
-- TODO: Uniqe ID (even if newest deleted)
--addBlog :: (MonadState AppState m) => Blogs -> Blog -> m ()
addBlog :: Blogs -> Blog -> Update AppState ()
addBlog bs b = do
        let maxID = foldl max 0 $ map blogID bs
        modBlogs $ bs++[b { blogID=maxID+1} ]

--deleteBlog :: (MonadState AppState m) => Blogs -> Int -> m ()
deleteBlog :: Blogs -> Int -> Update AppState ()
deleteBlog bs id = modBlogs $ filter (not.(==id).blogID) bs

--modBlogsLastUpdated :: (MonadState AppState m) => AppDate -> m ()
modBlogsLastUpdated :: AppDate -> Update AppState ()
modBlogsLastUpdated d = modify (\s -> s { blogsLastUpdated = d } )

--getBlogsLastUpdated :: (MonadReader AppState m) => m AppDate
getBlogsLastUpdated :: Query AppState AppDate
getBlogsLastUpdated = return.blogsLastUpdated =<< ask
