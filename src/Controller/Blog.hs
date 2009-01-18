module Controller.Blog where

import Prelude hiding (show)
import qualified Prelude as P
import HAppS.Server
import HAppS.State
import Control.Monad (liftM, liftM2, liftM5)
import Control.Monad.Trans (liftIO, MonadIO)
import System.Locale (defaultTimeLocale)
import Data.List (isInfixOf)
import Data.Time
import Data.Time.Format (parseTime)
import Text.StringTemplate (newSTMP)
import qualified GHC.Show as S
import Helper
import AppEnv
import AppState
import AppState.Types
import AppState.AppDate
import Model.Blog
import Model.Tag
import Model.User

-- PUBLIC 
blogController :: [ServerPartT IO Response]
blogController = [
                 dir "list" [multi list''],
                 dir "show" [withAuthentication.wrapLayout.path $ \id -> [anyRequest (show' id)]
                            ,wrapLayout.path $ \id -> [anyRequest (show id)]],
                 dir "edit" [path (\id -> [methodSP GET (withAuthentication.wrapLayout.anyRequest $ edit id), show'' GET id])],
                 dir "edit" [path (\id -> [methodSP POST (withAuthentication.withRequest $ edit' id), show'' POST id])],
                 dir "new" [withAuthentication (methodSP GET (wrapLayout.anyRequest $ new))
                           ,listOther GET],
                 dir "new" [withAuthentication (methodSP POST (withRequest new'))
                           ,listOther POST],
                 dir "delete" [path (\id -> [withAuthentication.anyRequest $ delete id, show'' GET id])],
                 dir "tag" [path (\id -> [wrapLayout.anyRequest $ tag id])],
                 dir "series" [path (\id -> [wrapLayout.anyRequest $ tag id])],
                 dirindex [multi list'']
                 ]
                 where list'' = [withAuthentication.wrapLayout.anyRequest $ list', wrapLayout.anyRequest $ list]
                       listOther m = methodSP m $ anyRequest (seeOther' "/blog/list")
                       show'' :: HAppS.Server.Method -> Int -> ServerPartT IO Response
                       show'' m id = methodSP m $ seeOther'' $ "/blog/show/"++(P.show id)
-- PRIVATE
type BlogTitle = String
type BlogBody = String
type BlogDate' = String
type BlogTime' = String
type BlogPrivate = String
data BlogInfo = BlogInfo BlogTitle BlogBody BlogDate' BlogTime' BlogPrivate
instance FromData BlogInfo where
         fromData = liftM5 BlogInfo (look "title") 
                                    (look "body") 
                                    (look "date") 
                                    (look "time") 
                                    (look "private")

list :: WebT IO LayoutResponse
list = liftIO $ do
     env           <- appEnv :: IO (AppEnv String String)
     blogs         <- liftM (reverse.sortByDate) $ query ListBlogs
     blogTemplate  <- liftM (attr "blogs" blogs) $ parseTemplate env "blogs_list"
     let title = attr "title" "Blog"
     returnLayout Nothing (attrSession' title "body" blogTemplate)

list' :: WebT IO LayoutResponse
list' = liftIO $ do
      env          <- appEnv
      blogs        <- liftM (reverse.sortByDate) $ query ListAllBlogs
      blogTemplate <- liftM (attr "blogs" blogs) $ parseTemplate env "blogs_list"
      let title = attr "title" "Blog"
      returnLayout Nothing (attrSession' title "body" blogTemplate)

show :: Int -> WebT IO LayoutResponse
show id  = liftIO $ do
     env                 <- appEnv :: IO (AppEnv String String)     
     blog                <- query $ GetBlog id
     blogTemplate        <- case blog of
          Nothing -> parseTemplate env "404"
          Just b  -> liftM (attr "blog" b
                           .attr "id" id) $ parseTemplate env "blogs_show" 
     let title = attr "title" $ maybe "Fannst ekki" blogTitle blog
     returnLayout Nothing (attrSession' title "body" blogTemplate)

show' :: Int -> WebT IO LayoutResponse
show' id  = liftIO $ do
     env                 <- appEnv :: IO (AppEnv String String)     
     blog                <- query $ GetBlog' id
     blogTemplate        <- case blog of
          Nothing -> parseTemplate env "404"
          Just b  -> liftM (attr "blog" b
                           .attr "id" id) $ parseTemplate env "blogs_show" 
     let title = attr "title" $ maybe "Fannst ekki" blogTitle blog
     returnLayout Nothing (attrSession' title "body" blogTemplate)

edit :: Int -> WebT IO LayoutResponse
edit id = liftIO $ do
     env                 <- appEnv :: IO (AppEnv String String)     
     blog                <- query $ GetBlog' id
     blogTemplate        <- case blog of
          Nothing -> parseTemplate env "404"
          Just b  -> liftM (attr "blog" b
                           .attr "id" id) $ parseTemplate env "blogs_edit" 
     let title = attr "title" $ maybe "Fannst ekki" blogTitle blog
     returnLayout Nothing (attrSession' title "body" blogTemplate)

edit' :: Int -> Request -> WebT IO Response
edit' id = \req -> do
      toWebT req $ withData' $ \(BlogInfo title body date time private) -> do
            toWebT req $ withSession $ \msdata -> do
                  let u = maybe NoUser user msdata
                  let publishedDate = maybe NoBlogDate toBlogDate 
                                      (parseTime defaultTimeLocale "%d.%m.%Y%H:%M:%S" (date++time))
                  blogs <- liftIO $ query ListAllBlogs
                  update $ ModBlogs $ (map (\b -> if blogID b == id 
                                                           then b { 
                                                                  blogTitle=title, 
                                                                  blogBody=body,
                                                                  blogPublishedDate=publishedDate,
                                                                  blogStatus=Published,
                                                                  blogPrivate=blogPrivate' private }
                                                           else b)) blogs
                  update . ModBlogsLastUpdated =<< liftM toAppDate (liftIO getCurrentTime)
                  seeOther' $ "/blog/show/" ++ (P.show id)
      
new :: WebT IO LayoutResponse
new = liftIO $ do
    env        <- appEnv :: IO (AppEnv String String)
    template   <- parseTemplate env "blogs_new"
    let title = attr "title" "N\195\189tt blog"
    returnLayout Nothing (attrSession' title "body" template)

new' :: Request -> WebT IO Response
new' = \req -> do
     (flip unServerPartT) req $ withData' $ \(BlogInfo title body date time private) -> do
           (flip unServerPartT) req $ withSession $ \msdata -> do
                 let u = maybe NoUser user msdata
                 let publishedDate = maybe NoBlogDate toBlogDate 
                                           (parseTime defaultTimeLocale "%d.%m.%Y%H:%M:%S" (date++time))
                 date' <- liftIO $ liftM toBlogDate getCurrentTime
                 let blog = Blog { 
                                 blogID=0, 
                                 blogTitle=title, 
                                 blogBody=body, 
                                 blogAuthor=u, 
                                 blogDate=date',
                                 blogPublishedDate=publishedDate,
                                 blogStatus=Published,
                                 blogPrivate=blogPrivate' private }
                 blogs <- liftIO $ query ListBlogs
                 update $ AddBlog blogs blog
                 update . ModBlogsLastUpdated =<< liftM toAppDate (liftIO getCurrentTime)
                 seeOther' "/blog/list" 

delete :: Int -> WebT IO Response
delete id = do
       blogs <- liftIO $ query ListBlogs
       update $ DeleteBlog blogs id
       update . ModBlogsLastUpdated =<< liftM toAppDate (liftIO getCurrentTime)
       seeOther' "/blog/list"


tag :: String -> WebT IO LayoutResponse
tag tag' = liftIO $ do
     env           <- appEnv :: IO (AppEnv String String)
     tag''         <- query $ GetTag tag'
     let blogIDS = maybe [] tagBlogs tag''
     blogs         <- liftM (reverse.sortByDate.filter ((`elem` blogIDS).blogID)) $ query ListBlogs
     blogTemplate  <- liftM (attr "blogs" blogs) $ parseTemplate env "blogs_list"
     let title = attr "title" "Blog"
     returnLayout Nothing (attrSession' title "body" blogTemplate)

blogPrivate' :: String -> Bool
blogPrivate' p = if p == "1" then True else False
