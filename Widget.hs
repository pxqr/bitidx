module Widget
       ( CommentView
       , BlankForm

         -- * Main Pages
       , homePage
       , searchPage
       , addReleasePage
       , releasePage

         -- * Add release page
       , NewRelease (..)
       , addForm
       , addedSuccessfullyMessage

         -- * Release subpages
         -- ** Description
       , Description (..)
       , descriptionForm
       , descriptionPage

         -- ** Metadata
       , metadataForm
       , metadataEditorPage

         -- ** Discussion
       , discussionPage

         -- * User
       , userProfilePage
       , userEditPage
       ) where

import Prelude
import Control.Applicative
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
import Network.URI
import Text.Markdown
import Text.Show
import Yesod as Y

import Foundation
import Model
import Model.Entities
import Settings

import Data.Torrent
import Data.Torrent.Layout
import Data.Torrent.InfoHash
import Data.Torrent.Magnet
import Data.Torrent.Piece

import Handler.User.Permissions


{-----------------------------------------------------------------------
--  Utils
-----------------------------------------------------------------------}

tinyUserpic :: GravatarOptions
tinyUserpic = def
  { gDefault = Just Identicon
  , gSize    = Just (Size 25)
  }

largeUserpic :: GravatarOptions
largeUserpic = def
  { gDefault = Just Identicon
  , gSize    = Just (Size 150)
  }

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

type BlankForm = (Widget, Enctype)
type CommentView = (Entity Comment, Entity User)

{-----------------------------------------------------------------------
--  Add release page
-----------------------------------------------------------------------}

data NewRelease = NewRelease
  { torrentName :: T.Text
  , torrentFile :: Y.FileInfo
  }

addForm :: Form NewRelease
addForm = renderDivs $ do
  NewRelease
    <$> areq textField "Name" Nothing
    <*> fileAFormReq   "Choose a .torrent file"

addReleasePage :: BlankForm -> Widget
addReleasePage (formWidget, formEnctype) = do
  setTitle "Add torrent to database"
  $(widgetFile "add/upload")

addedSuccessfullyMessage :: Html
addedSuccessfullyMessage = [shamlet|
    <div .label-success> Torrent successufully added to database.
  |]

{-----------------------------------------------------------------------
--  Description editor
-----------------------------------------------------------------------}

data Description = Description
  { name        :: T.Text
  , description :: Maybe Textarea
  }

descriptionForm :: Description -> Form Description
descriptionForm Description {..} = renderBootstrap $ do
  Description <$> areq textField     "Name"        (Just name)
              <*> aopt textareaField "Description" (Just description)

descriptionW :: BlankForm -> InfoHash -> Widget
descriptionW (widget, enctype) ih = $(widgetFile "torrent/description")

descriptionPage :: BlankForm -> InfoHash -> Widget
descriptionPage form ih = do
  setTitle "Release description"
  descriptionW form ih

{-----------------------------------------------------------------------
--  Metadata editor
-----------------------------------------------------------------------}
-- TODO more fields
-- TODO detalize fields
-- TODO use yesod forms for search form

announceInput :: FieldSettings App
announceInput = "Announce"

commentInput :: FieldSettings App
commentInput = FieldSettings
  { fsLabel   = "Comment"
  , fsTooltip = Just "tooltip - usage of this field"
  , fsId      = Just "commentId"
  , fsName    = Just "commentName"
  , fsAttrs   = [("placeholder", "freeform author comment")]
  }

createdByInput :: FieldSettings App
createdByInput = "Created by"

creationDateInput :: FieldSettings App
creationDateInput = "Creation date"

encodingInput :: FieldSettings App
encodingInput = "Encoding"

publisherURLInput :: FieldSettings App
publisherURLInput = "Publisher URL"

uriToText :: URI -> T.Text
uriToText = T.pack . show

-- note that text is always parsable so we can safely omit the Nothing case
textToURI :: T.Text -> URI
textToURI = maybe err id . parseURI . T.unpack
  where
    err = error "textToURL: impossible"

encodingField :: Field Handler T.Text
encodingField = textField

metadataForm :: Torrent -> Form Torrent
metadataForm Torrent {..} = renderBootstrap $ do Torrent
  <$> (textToURI <$> areq urlField announceInput (Just (uriToText tAnnounce)))
  <*> pure tAnnounceList
  <*> (fmap unTextarea <$> aopt textareaField commentInput (Just (Textarea <$> tComment)))
  <*> aopt textField createdByInput (Just tCreatedBy)
  <*> pure tCreationDate
  <*> aopt encodingField encodingInput (Just tEncoding)
  <*> pure tInfoDict
  <*> pure tPublisher
  <*> (fmap textToURI <$> aopt urlField publisherURLInput (Just (uriToText <$> tPublisherURL)))
  <*> pure tSignature

metadataEditorW :: BlankForm -> InfoHash -> Widget
metadataEditorW (widget, enctype) ih = $(widgetFile "torrent/metadata-edit")

metadataEditorPage :: BlankForm -> InfoHash -> Widget
metadataEditorPage form ih = do
  setTitle "Release metadata"
  metadataEditorW form ih

metadataW :: Torrent -> Widget
metadataW torrent = $(widgetFile "torrent/metadata")

{-----------------------------------------------------------------------
--  Main pages
-----------------------------------------------------------------------}

homePage :: [Release] -> Widget
homePage releases = do
  aDomId <- newIdent
  setTitle "BitIdx"
  $(widgetFile "homepage")
  releaseListW releases

discussionW :: BlankForm -> Release -> [CommentView] -> Permissions -> Widget
discussionW (formWidget, formEnctype) Release {..} comments Permissions {..} = do
  $(widgetFile "torrent/discussion")

discussionPage :: BlankForm -> Release -> [CommentView] -> Permissions -> Widget
discussionPage form release @ Release {..} comments permissions = do
  setTitle (toHtml releaseName)
  discussionW form release comments permissions

releaseW :: Release -> Permissions -> Widget
releaseW Release {..} Permissions {..} = $(widgetFile "torrent/release")

releasePage :: BlankForm -> Release -> [CommentView] -> Permissions -> Widget
releasePage form release @ Release {..} comments permissions = do
  setTitle (toHtml releaseName)
  releaseW release permissions
  discussionW form release comments permissions

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

{-----------------------------------------------------------------------
--  User pages
-----------------------------------------------------------------------}

userProfilePage :: User -> Widget
userProfilePage User {..} = do
  setTitle (toHtml userScreenName)
  $(widgetFile "user/profile")

userEditPage :: BlankForm -> User -> Widget
userEditPage (formWidget, formEnctype) User {..} = do
  setTitle (toHtml userScreenName)
  $(widgetFile "user/edit")

userMissingPage :: T.Text -> Widget
userMissingPage userName = do
  setTitle "Unknown User"
  $(widgetFile "user/missing")
