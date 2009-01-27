module Controller.Session (sessionController) where

import HAppS.Server
import HAppS.State
import AppState.Types
import AppEnv
import Control.Monad
import Control.Monad.Trans (liftIO)
import Helper
import AppState
import Text.StringTemplate (newSTMP)

sessionController :: [ServerPartT IO Response]
sessionController = withAuthentication' [
    dir "list" [list''],
    dir "delete" [path (\id -> [withCookie "sid" $ delete id])],
    dir "clean" [withCookie "sid" clean],
    dirindex [list'']
    ] 
    where list'' = wrapLayout.anyRequest $ list
    
list :: WebT IO LayoutResponse
list = liftIO $ do
     env           <- appEnv :: IO (AppEnv String String)
     sess          <- query ListSessionData
     sessTemplate  <- liftM (attr "sessions" sess) $ parseTemplate env "sessions_list"
     returnLayout Nothing (attrSession "body" sessTemplate)
     
delete :: SessionID -> SessionID -> WebT IO Response
delete id cookieID = do
       if cookieID /= id
          then update $ DelSession id
          else return ()
       seeOther' $ "/sessions/"

clean :: SessionID -> WebT IO Response
clean cookieID = do
      update $ CleanSessions cookieID
      seeOther' $ "/sessions/"