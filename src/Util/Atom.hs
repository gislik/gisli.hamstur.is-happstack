module Util.Atom (
                 Feedable, 
                 toFeed, 
                 toItem, 
                 showFeed
                 ) where

import Happstack.Server
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Char8 as B
import qualified Text.Atom.Feed as A
import Text.Feed.Export
import Text.Feed.Types
import Text.XML.Light
import Data.Time.Clock

class Feedable a where
      toFeed :: UTCTime -> a -> Feed
      toFeed _ x = AtomFeed $ A.nullFeed "" (A.TextString "") ""
      toItem :: a -> Item
      toItem x = AtomItem $ A.nullEntry "" (A.TextString "") ""

instance ToMessage Feed where
         toMessage feed  = L.pack . showTopElement .modTitleElement. xmlFeed $ feed
         toContentType _ = B.pack "text/xml;charset=utf-8"


showFeed :: Feed -> WebT IO Response
showFeed =  return.toResponse

-- PRIVATE --
modTitleElement :: Element -> Element
modTitleElement el = el { elContent = map (modContent el) (elContent el) }

modContent :: Element -> Content -> Content
modContent el cont = case cont of
                          Elem el'  -> Elem $ modTitleElement el'
                          Text cd | tagName el == "title" 
                                  || tagName el == "name" 
                                  || tagName el == "rights" -> Text $ modCDataVerbatim cd
                          otherwise -> cont 
                          where tagName = qName.elName

modCDataVerbatim :: CData -> CData
modCDataVerbatim cd = cd { cdVerbatim = CDataVerbatim } 
