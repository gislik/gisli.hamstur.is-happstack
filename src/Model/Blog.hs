{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, FlexibleInstances, TypeSynonymInstances, MultiParamTypeClasses #-}

module Model.Blog (
                  Blog(..), 
                  Blogs,
                  BlogID,
                  B1.BlogDate(B1.NoBlogDate), 
                  BlogStatus(..),
                  toBlogDate, 
                  fromBlogDate,
                  sortByDate
                  ) where

import Data.Time
import Text.StringTemplate
import Data.Map
import Data.Char (isNumber)
import Data.List (sortBy)
import Data.Generics
import Happstack.State
import Happstack.Data
import Text.Feed.Types (Feed(AtomFeed), FeedKind(AtomKind), Item(AtomItem))
import Text.Feed.Constructor (withFeedItems, atomEntryToItem)
import Text.Feed.Util (toFeedDateString)
import Text.Atom.Feed
import System.Time (ClockTime(TOD))
import System.Locale (defaultTimeLocale)
import AppEnv (title, author, url, feedUrl)
import Model.User
import Util.Atom

import qualified Model.Blog1 as B1

type BlogDate = B1.BlogDate

data BlogStatus = Published | Unpublished
         deriving (Show, Read, Data, Typeable, Eq)

instance Version BlogStatus
$(deriveSerialize ''BlogStatus)

type BlogID = Int

data Blog = Blog {
     blogID            :: BlogID,
     blogTitle         :: String,
     blogBody          :: String,
     blogDate          :: BlogDate,
     blogStatus        :: BlogStatus,
     blogPublishedDate :: BlogDate,
     blogPrivate       :: Bool,
     blogAuthor        :: User
     } deriving (Show, Read, Data, Typeable)

instance Migrate B1.Blog Blog where
         migrate b1 = Blog { blogID=(B1.blogID b1),
                             blogTitle=(B1.blogTitle b1),
                             blogBody=(B1.blogBody b1),
                             blogDate=(B1.blogDate b1),
                             blogStatus=Published,
                             blogPublishedDate=(B1.blogDate b1),
                             blogPrivate=False,
                             blogAuthor=(B1.blogAuthor b1)
                             }

instance Version Blog where
         mode = extension 2 (Proxy :: Proxy B1.Blog)
$(deriveSerialize ''Blog)

-- FUNCTIONS
toBlogDate :: UTCTime -> BlogDate
toBlogDate (UTCTime d dt) = B1.BlogDate (toModifiedJulianDay d) $ (read.takeWhile isNumber.show) dt

fromBlogDate :: BlogDate -> UTCTime
fromBlogDate (B1.BlogDate d s) =  UTCTime (ModifiedJulianDay d) (secondsToDiffTime s)
fromBlogDate B1.NoBlogDate = UTCTime (ModifiedJulianDay 0) (secondsToDiffTime 0)

sortByDate :: [Blog] -> [Blog]
sortByDate = sortBy (\b1 b2 -> compare (blogDate b1) (blogDate b2))


-- TEMPLATE
instance ToSElem Blog where
         toSElem b = (toSElem.fromList. Prelude.filter ((/=)"False".snd)) [("id", (show.blogID) b), 
                                         ("title", blogTitle b), 
                                         ("body", blogBody b), 
                                         ("date", (stringTemplateFormattedShow "%d.%m.%Y".fromBlogDate.blogPublishedDate) b), 
                                         ("time", (stringTemplateFormattedShow "%H:%M:%S".fromBlogDate.blogPublishedDate) b), 
                                         ("author", (name.blogAuthor) b),
                                         ("private", (show.blogPrivate) b)]

type Blogs = [Blog]

-- ATOM
instance Feedable Blogs where
         toFeed updated bs = withFeedItems (Prelude.map toItem bs) $ AtomFeed bs' { 
                                                                                    feedAuthors=[author'], 
                                                                                    feedLinks=[link'], 
                                                                                    feedRights=Just rights' 
                                                                                    }
                             where bs' = (nullFeed "tag:gisli.hamstur.is" 
                                         (HTMLString title) 
                                         (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" updated))
                                   link' = Link { 
                                                  linkHref      = feedUrl, 
                                                  linkRel       = (Just . Left) "self", 
                                                  linkType      = Nothing, 
                                                  linkHrefLang  = Nothing,
                                                  linkTitle     = Nothing,
                                                  linkLength    = Nothing,
                                                  linkAttrs     = [],
                                                  linkOther     = []
                                                  }
instance Feedable Blog where
         toItem b = 
                    AtomItem $ b' { 
                                    entryAuthors=[author'], 
                                    entryContent=Just (HTMLContent (blogBody b)), 
                                    entryLinks=[link' b], 
                                    entryRights=Just rights' 
                                    }
                             where b' = nullEntry ("tag:gisli.hamstur.is,"
                                                     ++(stringTemplateFormattedShow "%Y-%m-%d".fromBlogDate.blogDate) b
                                                     ++":"
                                                     ++(show.blogID) b)
                                                    (TextString (blogTitle b))
                                                    ((formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ".fromBlogDate.blogDate) b)
                                   link' b = Link {
                                                    linkHref      = url++"blog/show/"++(show.blogID) b,
                                                    linkRel       = (Just . Left) "alternate", 
                                                    linkType      = Nothing, 
                                                    linkHrefLang  = Nothing,
                                                    linkTitle     = Nothing,
                                                    linkLength    = Nothing,
                                                    linkAttrs     = [],
                                                    linkOther     = []
                                                    }

author' :: Person
author' = Person { personName=author, personURI=Just url, personEmail=Nothing, personOther=[] }

rights' :: TextContent
rights' = HTMLString $  "&copy;2008 "++author++" &nbsp;"
