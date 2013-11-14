module Widget
       ( CommentView
       , descriptionW
       , metadataW
       , metadataEditorW
       , discussionW
       , releasePage

       , searchW
       , releaseListW
       , searchPage
       ) where

import Prelude
import Control.Monad
import Data.Bool
import Data.Functor
import Data.List as L
import Data.Maybe
import Data.Monoid
import Data.Text as T
import Data.Text.Encoding as T
import Data.Text.Lazy as TL
import Data.Text.Lazy.Builder as TL
import Data.Text.Lazy.Builder.Int as TL
import Data.Text.Lazy.Builder.RealFloat as TL
import Data.Time.Clock.POSIX -- TODO local time
import Network.Gravatar
import Text.Markdown
import Text.Show
import Yesod

import Foundation
import Model
import Model.Entities
import Settings as Settings

import Data.Torrent
import Data.Torrent.Layout
import Data.Torrent.InfoHash
import Data.Torrent.Magnet
import Data.Torrent.Piece

import Handler.User.Permissions


type BlankForm = (Widget, Enctype)

showSize :: Integral a => a -> TL.Text
showSize = toLazyText . truncate
  where
    truncate i
      | i >= gib
      , t <- fromIntegral i / fromIntegral gib :: Double
      = formatRealFloat Fixed (Just 3) t  <> "GiB"
      | i >= mib  = decimal (i `div` mib) <> "MiB"
      | i >= kib  = decimal (i `div` kib) <> "KiB"
      | otherwise = decimal i <> "B"

    (gib, mib, kib) = (1024 ^ 3, 1024 ^ 2, 1024 ^ 1)

type CommentView = (Entity Comment, Entity User)

descriptionW :: BlankForm -> InfoHash -> Widget
descriptionW (widget, enctype) ih = $(widgetFile "torrent/description")

metadataW :: Torrent -> Widget
metadataW torrent = $(widgetFile "torrent/metadata")

metadataEditorW :: BlankForm -> InfoHash -> Widget
metadataEditorW (widget, enctype) ih = $(widgetFile "torrent/metadata-edit")

discussionW :: BlankForm -> Release -> [CommentView] -> Permissions -> Widget
discussionW (formWidget, formEnctype) Release {..} comments Permissions {..} = do
  $(widgetFile "torrent/discussion")

releaseW :: Release -> Permissions -> Widget
releaseW Release {..} Permissions {..} = $(widgetFile "torrent/release")

releasePage :: Release -> Permissions -> Widget
releasePage release @ Release {..} permissions = do
  setTitle (toHtml releaseName)
  releaseW release permissions
  -- TODO add discussion here.

releaseListW :: [Release] -> Widget
releaseListW releases = $(widgetFile "torrent/list")

searchW :: Maybe T.Text -> Widget
searchW msearchString = $(widgetFile "search")

searchPage :: Maybe T.Text -> [Release] -> Widget
searchPage msearchString releases = do
  setTitle "Search — Bitidx"
  searchW msearchString
  when (isJust msearchString) $ do
    releaseListW releases
