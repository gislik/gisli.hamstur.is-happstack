{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, FlexibleInstances, TypeSynonymInstances #-}

module Model.Blog1 (
                  Blog(..), 
                  Blogs, 
                  BlogDate(BlogDate,NoBlogDate), 
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
import HAppS.State
import HAppS.Data
import Text.Atom.Feed as A
import Text.Feed.Types (Feed(AtomFeed), FeedKind(AtomKind), Item(AtomItem))
import Text.Feed.Util (toFeedDateString)
import Text.Feed.Constructor (withFeedItems, atomEntryToItem)
import System.Time (ClockTime(TOD))
import AppEnv
import Model.User
--import Util.Atom

-- MODEL
data BlogDate = BlogDate Integer Integer | NoBlogDate
        deriving (Show, Read, Data, Typeable, Ord, Eq)

instance Version BlogDate
$(deriveSerialize ''BlogDate)

data BlogStatus = Published | Unpublished
         deriving (Show, Read, Data, Typeable, Ord, Eq)

instance Version BlogStatus
$(deriveSerialize ''BlogStatus)

data Blog = Blog {
     blogID          :: Int,
     blogTitle       :: String,
     blogBody        :: String,
     blogDate        :: BlogDate,
--     blogStatus      :: BlogStatus,
--     blogPublishDate :: BlogDate,
--     blogPrivate     :: Bool,
     blogAuthor      :: User
     } deriving (Show, Read, Data, Typeable)

instance Version Blog
$(deriveSerialize ''Blog)

-- FUNCTIONS
toBlogDate :: UTCTime -> BlogDate
toBlogDate (UTCTime d dt) = BlogDate (toModifiedJulianDay d) $ (read.takeWhile isNumber.show) dt
fromBlogDate :: BlogDate -> UTCTime
fromBlogDate (BlogDate d s) =  UTCTime (ModifiedJulianDay d) (secondsToDiffTime s)
fromBlogDate NoBlogDate = UTCTime (ModifiedJulianDay 0) (secondsToDiffTime 0)

toClockTime :: BlogDate -> ClockTime
toClockTime (BlogDate d dt) = TOD (60*60*24*d+((read.takeWhile isNumber.show) dt)) 0

sortByDate :: [Blog] -> [Blog]
sortByDate = sortBy (\b1 b2 -> compare (blogDate b1) (blogDate b2))


-- TEMPLATE
instance ToSElem Blog where
         toSElem b = (toSElem.fromList) [("id", (show.blogID) b), 
                                         ("title", blogTitle b), 
                                         ("body", blogBody b), 
                                         ("date", (stringTemplateFormattedShow "%d.%m.%Y".fromBlogDate.blogDate) b), 
                                         ("time", (stringTemplateFormattedShow "%H:%M:%S".fromBlogDate.blogDate) b), 
                                         ("author", (name.blogAuthor) b)]

type Blogs = [Blog]

{-
instance Feedable Blog where
         toItem b = AtomItem $ A.nullEntry (blogTitle b) 
                                           (HTMLString (blogBody b)) 
                                           (toFeedDateString AtomKind ((toClockTime.blogDate) b))

instance Feedable Blogs where
         toFeed bs = withFeedItems (Prelude.map toItem bs) $ AtomFeed (A.nullFeed "ID" 
                                                                                  (TextString title) 
                                                                                  "Last updated data")
--}