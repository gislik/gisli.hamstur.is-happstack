module Main where

import Happstack.Server
import Happstack.State
import Control.Concurrent
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans (liftIO)
import AppState
import AppState.Types
import qualified AppEnv
import Helper
import Controller.Blog
import Controller.User
import Controller.Atom
import Controller.Session
import Text.StringTemplate (newSTMP)

main :: IO ()
main = do
     putStrLn "Starting ..."
----     control <- startSystemState entryPoint
     tid <- forkIO $ simpleHTTP Conf {port=5000, validator=Nothing} $ --msum [dir "test" (return "hello world")]
                   (msum $ [
                   dir "users" userController,
                   dir "blog" blogController,
                   dir "atom" atomController,
                   dir "sessions" sessionController,
                   dir "proxy" (proxyServe ["google.com","*.google.com"]),
                   dir "src" (sourceServe [] "src"),
                   dirindex [withRequest void],
                   fileServe' ["index.html"] "public"])
--     putStrLn "Creating checkpoint"
--     createCheckpoint control
     putStrLn "Started"
     waitForTermination
     putStrLn "Shutting down..."
     killThread tid
----     shutdownSystem control
     putStrLn "Shutdown complete"

entryPoint :: Proxy AppState
entryPoint = Proxy

handleTitle :: ReaderT ([(String, Input)], [(String, Cookie)]) Maybe String
handleTitle = do
            pairs <- lookPairs
            return $ case pairs of
                   (("me",_):_)       -> "\195\137g"
                   (("blog",_):_)     -> "Blog"
                   (("albums",_):_)   -> "Myndir"
                   (("projects",_):_) -> "Verkefni"
                   (("about",_):_)    -> "T\195\166knil\195\189sing"
                   (("english",_):_)  -> "Welcome"
                   otherwise          -> AppEnv.title

void :: Request -> WebT IO Response
void req = do
     withDataFn' req handleTitle $ \title -> do                 
         toWebT req.wrapLayout.liftIO $ do
             returnLayout Nothing (attrSession' (attr "title" title) "body" (newSTMP ""))
