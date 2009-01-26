module Controller.User (userController)  where

import HAppS.Server
import HAppS.State
import Control.Monad.Reader
import Network.URI (URI, parseURI, nullURI)
import Network.Curl
import Text.JSON (JSValue(JSObject, JSString), fromJSObject, fromJSString)
import Text.JSON.Parsec hiding (token)
import Data.Map (Map)
import Data.Char (isAlphaNum)
import Data.Maybe
import Helper
import AppEnv
import AppState.Types
import AppState
import Data.List (intercalate)
import Model.User

-- PUBLIC
userController :: [ServerPartT IO Response]
userController = [
                 dir "login" [wrapLayout $ methodSP GET (anyRequest login)],
                 dir "login" [methodSP POST $ withData' $ login' redirect'],
                 dir "token" [withData' (token redirect')],
                 dir "logout" [withCookie "sid" $ logout redirect', redirect'']
                 ]
                 where redirect' = seeOther' "/"
                       redirect'' = anyRequest redirect'
                       

-- PRIVATE
readAppKey :: IO String
readAppKey = liftM (filter isAlphaNum) (readFile "rpxnow.key")

type UserName = String
type Password = String
data LoginInfo = LoginInfo UserName Password
instance FromData LoginInfo where
         fromData = liftM2 LoginInfo (look "username") (look "password")

data Token = Token String
instance FromData Token where
         fromData = liftM Token (look "token")

login :: WebT IO LayoutResponse
login = do
      liftIO $ do
             env <- appEnv :: IO (AppEnv String String)
             loginForm  <- parseTemplate env "login"
             returnLayout Nothing (attrSession "body" loginForm)

login' :: WebT IO Response -> LoginInfo -> WebT IO Response
login' action = \(LoginInfo username password) -> do
       user <- query $ AuthUser username password
       case user of
            Just user -> do
                 sid <- update $ (NewSession . SessionData) user
                 addCookie (-1) $ mkCookie "sid" sid -- this show is mighty important!
                 action
            Nothing -> respond "Not correct"

token :: WebT IO Response -> Token -> WebT IO Response
token action = \(Token token) -> do
      appKey <- liftIO readAppKey
      curl <- liftIO initialize
      body <- liftIO $ liftM respBody $ do_curl curl "https://rpxnow.com:443/api/v2/auth_info" [CurlPostFields ["apiKey=" ++ appKey, "token=" ++ token]]
      let profile = (maybe [] fromJSObject' . Prelude.lookup "profile".either (const []) id.parse p_object "https://rpxnow.com:443/api/v2/auth_info") body
      let primaryKey = (maybe "" fromJSString' . Prelude.lookup "primaryKey") profile
      user <- query $ GetUser primaryKey
      case user of
           Nothing      -> respond body
           Just user    -> do
                sid <- update $ (NewSession . SessionData) user
                addCookie (-1) $ mkCookie "sid" $ show sid -- this show is mighty important!
                action
      where fromJSObject' (JSObject v) = fromJSObject v
            fromJSObject' _            = []
            fromJSString' (JSString s) = fromJSString s
            fromJSString' _            = ""
                 
logout :: WebT IO Response -> SessionID -> WebT IO Response
logout action = \sid -> do
       update $ DelSession sid
       addCookie 0 $ mkCookie "sid" "0"
       action
