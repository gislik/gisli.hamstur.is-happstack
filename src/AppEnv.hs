module AppEnv where

import Text.StringTemplate
title :: String
title = "G\xc3\xadsli....! J\xc3\xa1, hva\xc3\xb0?"

author :: String
author = "G\xc3\xadsli Kristj\xc3\xa1nsson"

url :: String
url = "http://gisli.hamstur.is/"

feedUrl :: String
feedUrl = "http://feeds.feedburner.com/gisli-hamstur-is"

data AppEnv a b = AppEnv {
     baseGroup     :: STGroup a,
     globalSElem   :: [(String, b)]
     }
	
appEnv ::  (Stringable a) => IO (AppEnv a String)
appEnv  = do
            templates	<- directoryGroupLazy "templates" 		
            special	<- directoryGroupLazy "templates/special"
            let baseGroup = addSuperGroup templates special
            return $ AppEnv {baseGroup = baseGroup, globalSElem = [
                                                                        ("author", author), 
                                                                        ("url", url), 
                                                                        ("feedUrl", feedUrl)
                                                                        ]}
