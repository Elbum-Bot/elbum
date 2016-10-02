{-# LANGUAGE DeriveGeneric #-}

module AlbumTypes
( ImgSrc(..)
, Image(..)
, Album(..)
) where

import Data.Aeson
import GHC.Generics

{-| A photo album has a title and a collection of images.  Future: add
    sub-albums. -}
data Album
   = Album
   { title :: String
   , images :: [Image]
   } deriving (Generic, Show, Eq)

{-| Each image in the album has alt-text and a srcset.  The srcset
    aligns with the html <img/> tag's srcset attribute, which is
    basically just a list of the same image at different
    resolutions. -}
data Image
   = Image
   { altText :: String
   , srcSet :: [ImgSrc]
   } deriving (Generic, Show, Eq)

{-| A single image source, which comes from a particular URL and has a
    known fixed size. -}
data ImgSrc
   = ImgSrc
   { url :: String
   , x :: Int
   , y :: Int
   } deriving (Generic, Show, Eq)
