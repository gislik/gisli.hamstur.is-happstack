{-# LANGUAGE FlexibleInstances #-} 

module Helper where

import HAppS.Server
import HAppS.State
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Char8 as B
import Text.StringTemplate
import Control.Monad.Reader
import Data.List (intercalate, isInfixOf, isSuffixOf)
import System.Directory (getDirectoryContents, getCurrentDirectory, doesDirectoryExist)
import System.FilePath (combine, takeExtension, hasExtension)
import Language.Haskell.HsColour
import Language.Haskell.HsColour.ColourHighlight
import Data.Monoid
import AppEnv
import AppState
import AppState.Types
import Model.User

-- HAppS
newtype Html a = Html a
instance ToMessage (Html String) where
         toMessage (Html s) 	= L.pack s
         toContentType _ 	= B.pack "text/html"

instance ToMessage (Html L.ByteString) where
         toMessage (Html bs)    = bs
         toContentType _        = B.pack "text/html"

renderResponse :: (Stringable a) => StringTemplate a -> IO Response
renderResponse = return . toResponse . Html . stToString . render

toWebT :: Request -> ServerPartT m a -> WebT m a
toWebT = flip unServerPartT

withData' :: (Monad m, FromData a) => (a -> WebT m r) -> ServerPartT m r
withData' controller = withData (\d -> [anyRequest (controller d)])

withDataFn' :: (Monad m) => Request -> RqData a -> (a -> WebT m r) -> WebT m r
withDataFn' req rqData  = (flip unServerPartT) req . withDataFn'' rqData 

withDataFn'' :: (Monad m) => RqData a -> (a -> WebT m r) -> ServerPartT m r
withDataFn'' rqData controller = withDataFn rqData $ \d -> [anyRequest (controller d)]

withCookie_OLD :: (Monad m, Read a) => String -> (a -> WebT m r) -> ServerPartT m r
withCookie_OLD = withDataFn'' . readCookieValue

-- re-implemented withCookie to be able to use cookieFixer
withCookie :: (Monad m) => String -> (String -> WebT m r) -> ServerPartT m r
withCookie c handle = withRequest $ \req -> case lookup c (rqCookies req) of
                                            Nothing -> noHandle
                                            Just a  -> handle $ cookieValue a

withJustCookie name = withDataFn'' $ liftM Just (readCookieValue name) `mplus` return Nothing            

readData :: RqData a -> Request -> Maybe a
readData rqDataReader rq = runReaderT rqDataReader $ (rqInputs rq, rqCookies rq)

dirindex :: (Monad m) => [ServerPartT m a] -> ServerPartT m a
dirindex sps = spsIf pr sps
         where pr :: Request -> Bool
               pr req = 
                  case (length.rqPaths) req of
                       0 -> True
                       otherwise -> False
          
spsIf :: (Monad m) => (Request -> Bool) -> [ServerPartT m a] -> ServerPartT m a
spsIf p sps = withRequest $ \req ->
         if p req
            then unServerPartT (mconcat sps) req
            else mempty

seeOther' :: (Monad m) => String -> WebT m Response
seeOther' url =  seeOther url . toResponse $ "Redirecting"

seeOther'' :: (Monad m) => String -> ServerPartT m Response
seeOther'' = anyRequest . seeOther'


respond :: (Monad m) => String -> WebT m Response
respond = return . toResponse . Html

showResponse :: (Monad m, Show a) => a -> WebT m Response
showResponse = respond . show

fileServe' :: (MonadIO m) => [FilePath] -> FilePath -> ServerPartT m Response
fileServe' idxs dir = withRequest $ \req -> do
           let res = applyRequest [fileServe idxs dir] req ::  Either (IO Response) String
           res' <- liftIO $ either id (return.toResponse) res
           let isHtmFile = isSuffixOf ".htm" (rqUri req)
           let res'' = if isHtmFile 
                          then do
                               toResponse.Html $ rsBody res'
                          else res'
           return res''
            
sourceServe :: (MonadIO m) => [FilePath] -> FilePath -> ServerPartT m Response
sourceServe idxs dir = withRequest $ \req -> do
            let res = applyRequest [fileServe idxs dir] req ::  Either (IO Response) String
            res' <- liftIO $ either (handleResponse req) (return.id) res
            return $ (toResponse . Html) res'
                  
            where handleResponse :: Request -> IO Response -> IO String
                  handleResponse req res = do 
                                 code <- liftM rsCode res
                                 case code of
                                      403       -> handleDirectory req res
                                      otherwise -> colorResponse res
                  colorResponse :: IO Response -> IO String
                  colorResponse = liftM (hscolour HTML defaultColorPrefs True True False "Title" . L.unpack . rsBody)
                  handleDirectory :: Request -> IO Response -> IO String
                  handleDirectory req orgRes = do
                                  currentDir <- getCurrentDirectory
                                  let uri = rqUri $ req
                                  let dir = combine currentDir (drop 1 uri)
                                  let dotdot = isInfixOf ".." dir
                                  directoryExists <- doesDirectoryExist dir
                                  if directoryExists && not dotdot
                                     then (return.formatContent uri) =<< liftM (filter hsOrDir) (getDirectoryContents dir)
                                     else colorResponse orgRes
                  hsOrDir :: FilePath -> Bool
                  hsOrDir entry = ((==".hs").takeExtension) entry || (not.hasExtension) entry
                  formatContent :: FilePath -> [FilePath] -> String
                  formatContent dir files = do
                                let linkedfiles = map (linkify dir) files
                                intercalate br $ linkedfiles
                  linkify :: FilePath -> FilePath -> String
                  linkify dir file = "<a href=\""++(combine dir file)++"\">"++file++"</a>"
                  br :: String
                  br = "<br/>\n"

defaultColorPrefs =  ColourPrefs
  { keyword  = [Underscore,Foreground Green]
  , keyglyph = [Foreground Red]
  , layout   = [Foreground Cyan]
  , comment  = [Foreground Red]
  , conid    = [Normal]
  , varid    = [Normal]
  , conop    = [Bold,Foreground Red]
  , varop    = [Foreground Cyan]
  , string   = [Foreground Magenta]
  , char     = [Foreground Magenta]
  , number   = [Foreground Magenta]
  , cpp      = [Dim,Foreground Magenta]
  , selection = [Bold, Foreground Magenta]
  , variantselection = [Dim, Foreground Red, Underscore]
  , definition = [Foreground Blue]
  }            

-- TODO:
-- only wrap if has content (200)
-- Copy original response object
wrapLayout :: ServerPartT IO LayoutResponse -> ServerPartT IO Response
wrapLayout sp = do
           withRequest $ \req -> do
                       lresp  <- unServerPartT sp $ req
                       let attrs = layAttrs lresp                       
                       env    <- liftIO appEnv :: (WebT IO) (AppEnv String String)                        
                       sdata  <- query $ GetSession $ sid req
                       case sdata of 
                            Just d  -> do
                                 let user' = Just (user d)
                                 let attr' = attr "s" sdata  . attr "user" user'
                                 layoutTemplate <- liftM attr' (parseLayout env)
                                 liftIO.renderResponse $ attrs sdata user' layoutTemplate
                            Nothing -> do 
                                    layoutTemplate <- parseLayout env
                                    liftIO.renderResponse $ attrs Nothing Nothing layoutTemplate
-- Had to be rewritten because of cookieFixer
--            where sid req = maybe "" id $ readData ((readCookieValue "sid")::RqData SessionID) req
           where sid req = maybe "" cookieValue $ lookup "sid" (rqCookies req)
              


wrapFilter :: [ServerPartT IO LayoutResponse] -> [ServerPartT IO Response]
wrapFilter = Prelude.map wrapLayout

withSession :: (Maybe SessionData -> WebT IO a) 
                      -> ServerPartT IO a
withSession sessionController = withCookie "sid" $ \sid -> do
            session <- query $ GetSession sid
            sessionController session

withAuthentication :: ServerPartT IO Response -> ServerPartT IO Response
withAuthentication sp = do
                   withRequest $ \req -> do
                               unServerPartT (withSession (sess req)) req
                   where sess req (Just sdata) = do
                              uid <- query $ GetUser ((username.user) sdata)
                              maybe unauthResponse (const (unServerPartT sp req)) uid
                         sess _ Nothing = unauthResponse
                         unauthResponse = unauthorized.toResponse $ "unauthorized"

withAuthentication' :: [ServerPartT IO Response] -> [ServerPartT IO Response]
withAuthentication' = map withAuthentication

-- TEMPLATE
data LayoutResponse = LayoutResponse {
                               layResponse :: Maybe (WebT IO Response),
                               layAttrs    :: Maybe SessionData -> Maybe User -> StringTemplate String -> StringTemplate String
                               }

instance ToSElem (StringTemplate String) where
         toSElem = toSElem . render

returnLayout :: Maybe (WebT IO Response) -> (Maybe SessionData -> Maybe User -> StringTemplate String -> StringTemplate String) -> IO LayoutResponse
returnLayout lr la =  return $ LayoutResponse lr la

parseTemplate :: (Stringable a, ToSElem b,  Monad m) => 
              AppEnv a b -> 
              String -> 
              m (StringTemplate a)
parseTemplate env templ = liftM (setManyAttrib (globalSElem env)) (parseTemplate' (baseGroup env) templ)

parseTemplate' :: (Stringable a, Monad m) => 
               STGroup a -> 
               String -> 
--               [(String, b)] -> 
               m (StringTemplate a)
parseTemplate' group templ = 
	case getStringTemplate templ group of
		Nothing -> fail $ "Could not open template: " ++ templ
		Just template -> return template

parseLayout :: (Stringable a,  Monad m) =>
               AppEnv a String -> m (StringTemplate a)
parseLayout env = parseTemplate env "layout"

renderLayout :: (Stringable a) =>
                AppEnv a String -> 
                Maybe (StringTemplate a -> StringTemplate a) -> 
                IO Response
renderLayout env attrs = do
             layoutTemplate 	<- parseLayout env
             renderResponse $ maybe id id attrs layoutTemplate

attr :: (Stringable b, ToSElem a) =>
                String -> a -> StringTemplate b -> StringTemplate b
attr = setAttribute

attrSession :: (Stringable b) =>
                String -> 
                StringTemplate String -> 
                Maybe SessionData -> 
                Maybe User -> 
                StringTemplate b -> 
                StringTemplate b
--attrSession s a sess usr = attr s (attr "user" usr (attr "s" sess a))
attrSession = attrSession'' Nothing

attrSession' :: (Stringable b) =>
                    (StringTemplate b -> StringTemplate b) ->
                    String -> 
                    StringTemplate String -> 
                    Maybe SessionData -> 
                    Maybe User -> 
                    StringTemplate b -> 
                    StringTemplate b
attrSession' attrs = attrSession'' (Just attrs)

attrSession'' :: (Stringable b) =>
                    Maybe (StringTemplate b -> StringTemplate b) ->
                    String -> 
                    StringTemplate String -> 
                    Maybe SessionData -> 
                    Maybe User -> 
                    StringTemplate b -> 
                    StringTemplate b
attrSession'' attrs s a sess usr = maybe id id attrs.attr s (attr "user" usr (attr "s" sess a))
