{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, TypeSynonymInstances #-}
module Model.Tag where

import Happstack.State
import Model.Blog(BlogID)
import Data.Generics
import Data.Map

type TagName = String

data Tag = Tag {
    tagSeries :: Bool,
    tagBlogs  :: [BlogID],
    tagCount  :: Integer
} deriving (Eq, Show, Read, Data, Typeable)

instance Version Tag
$(deriveSerialize ''Tag)

type Tags = Map String Tag

{- data Tags = Tags {
    untag :: Map TagName Tag
} deriving (Eq, Show, Read, Data, Typeable)
instance Version Tags
$(deriveSerialize ''Tags)
-}

