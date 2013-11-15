{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS -fno-warn-orphans  #-}
module Handler.Release
       ( getInvalidTorrentFileR
       , getAlreadyExistR

       , getAddR
       , postAddR

       , getReleaseR
       , postReleaseR
       , deleteReleaseR
       , handleReleaseCommentR

       , getDiscussionR
       , postDiscussionR

       , getDescriptionR
       , postDescriptionR

       , getMetadataR
       , postMetadataR

       , getTorrentFileR
       ) where

import Import as I
import Data.Aeson.TH
import Data.BEncode as BE
import Data.ByteString as BS
import Data.ByteString.Base16 as Base16
import Data.ByteString.Lazy as BL
import Data.Conduit
import Data.Conduit.Lazy
import Data.List as L
import Data.Maybe
import Data.Serialize as S
import Data.Torrent
import Data.Torrent.InfoHash
import Data.Torrent.Layout
import Data.Torrent.Piece
import Data.Torrent.Magnet
import Data.Text as T
import Data.Text.Encoding as T
import Data.Text.Lazy as TL
import Data.Time
import Data.Time.Clock.POSIX -- TODO local time
import qualified Database.Esqueleto as Sql
import Network.URI
import Text.Markdown
import System.Locale
import Yesod.Auth
import Yesod.AtomFeed
import Yesod.RssFeed
import Model.Entities
import Settings as Settings
import Network.Gravatar
import Widget
import Handler.User.Permissions


withRelease :: InfoHash -> (Entity Release -> Handler a) -> Handler a
withRelease ih action = do
  merelease <- runDB $ getBy (UniqueRelease ih)
  maybe notFound action merelease

modifyRelease :: InfoHash -> Permission
              -> (Entity Release -> Handler a) -> Handler a
modifyRelease ih permission action = do
  withRelease ih $ \ (e @ (Entity _ Release {..})) -> do
    reqPermission permission releaseAuthor (action e)

{-----------------------------------------------------------------------
--  Error handlers
-----------------------------------------------------------------------}

torrentNotExist :: Handler Html
torrentNotExist = defaultLayout $ do
  [whamlet|
   <p .error> This torrent do not exist or probably has been deleted by the author.
   |]

getInvalidTorrentFileR :: Handler Html
getInvalidTorrentFileR = do
  defaultLayout $ do
    setTitle "Error: invalid torrent file"
    $(widgetFile "error/invalid-file")

getAlreadyExistR :: InfoHash -> Handler Html
getAlreadyExistR ih = do
  defaultLayout $ do
    setTitle "Torrent already exist"
    $(widgetFile "error/already-exist")

{-----------------------------------------------------------------------
--  Add new release
-----------------------------------------------------------------------}

getAddR :: Handler Html
getAddR = do
  form <- generateFormPost addForm
  defaultLayout $ addReleasePage form

postAddR :: Handler Html
postAddR = do
  authorId <- requireAuthId
  ((result, formWidget), formEnctype) <- runFormPost addForm
  withFormResult result $ \ (NewRelease name fileStream) -> do
    uploadedFileChunks <- liftIO $ runResourceT $ lazyConsume $ fileSource fileStream
    case BE.decode (BS.concat uploadedFileChunks) :: BE.Result Torrent of
      Left  msg     -> redirect InvalidTorrentFileR
      Right torrent -> do
        let ih = idInfoHash $ tInfoDict torrent
        added <- runDB $ addNewReleaseT name authorId torrent
        if not (isJust added)
          then redirect (AlreadyExistR ih)
          else do
               setMessage addedSuccessfullyMessage
               redirect $ ReleaseR ih

addNewReleaseT :: T.Text -> UserId -> Torrent -> YesodDB App (Maybe (Key Release))
addNewReleaseT name authorId torrent = do
  let ih = idInfoHash $ tInfoDict torrent
  mrelease <- getBy $ UniqueRelease ih
  let alreadyExist = isJust mrelease
  if alreadyExist
    then return Nothing
    else Just <$> do
      time <- liftIO $ getCurrentTime
      Sql.insert $ Release ih name authorId torrent Nothing time

{-----------------------------------------------------------------------
-- Release
-----------------------------------------------------------------------}

type ReleasePresentation a = Release -> [CommentView] -> Handler a

-- TODO show release author info
type ReleaseInfo = (Release, User, CommentView)

commentToEntry :: CommentView -> FeedEntry (Route App)
commentToEntry (Entity commentId Comment {..}, Entity _ User {..}) = FeedEntry
  { feedEntryLink    = ReleaseCommentR commentTorrentId commentId
  , feedEntryUpdated = commentAdded
  , feedEntryTitle   = userScreenName <> " wrote: " <> T.take 16 commentBody
  , feedEntryContent = markdown def $ TL.fromStrict commentBody
  }

releaseToFeed :: Release -> [CommentView] -> Feed (Route App)
releaseToFeed release @ Release {..} comments = Feed
  { feedTitle       = releaseName
  , feedLinkSelf    = ReleaseR releaseTorrentId
  , feedLinkHome    = HomeR
  , feedAuthor      = "someone" -- TODO
  , feedDescription = fromMaybe "No description" $ do
      desc <- releaseDescription
      return $ markdown def $ TL.fromStrict desc

  , feedLanguage    = "en"
  , feedUpdated     = let times = lastUpdated release : fmap (lastUpdated . fst) comments
                      in L.maximum times
  , feedEntries     = commentToEntry <$> comments
  }

getReleaseRss :: ReleasePresentation RepRss
getReleaseRss release comments = rssFeed (releaseToFeed release comments)

getReleaseAtom :: ReleasePresentation RepAtom
getReleaseAtom release comments = atomFeed (releaseToFeed release comments)

feedLink :: Route App -> T.Text -> Widget
feedLink   route title = do
  atomLink route $ "RSS: "  <> title
  rssLink  route $ "Atom: " <> title

getReleaseJson :: ReleasePresentation Value
getReleaseJson release comments = pure $ object
  [ "comments" .= False -- fmap entityVal comments
  ]

getReleaseHtml :: ReleasePresentation Html
getReleaseHtml release @ Release {..} comments = do
  permissions <- getPermissions releaseAuthor
  form  <- generateFormPost commentForm
  defaultLayout $ do
    feedLink (ReleaseR releaseTorrentId) releaseName
    releasePage form release comments permissions

selectComments :: InfoHash -> YesodDB App [CommentView]
selectComments infohash =
  Sql.select $
  Sql.from $ \ (comment `Sql.InnerJoin` user) -> do
    Sql.on (comment Sql.^. CommentAuthor Sql.==. user Sql.^. UserId)
    Sql.where_ (comment Sql.^. CommentTorrentId Sql.==. Sql.val infohash)
    return (comment, user)

getReleaseR :: InfoHash -> Handler TypedContent
getReleaseR ih = do
  (metorrent, comments) <- runDB $ do
    (,) <$> getBy      (UniqueRelease ih)
        <*> selectComments ih

  case metorrent of
    Nothing      -> notFound
    Just (Entity _ release) -> selectRep $ do
      provideRep $ getReleaseRss  release comments
      provideRep $ getReleaseAtom release comments
      provideRep $ getReleaseHtml release comments
      provideRep $ getReleaseJson release comments

postReleaseR :: InfoHash -> Handler Html
postReleaseR = deleteReleaseR

deleteReleaseR :: InfoHash -> Handler Html
deleteReleaseR ih = do
  withRelease ih $ \ (Entity releaseId Release {..}) -> do
    reqPermission editor releaseAuthor $ do
       runDB (I.delete releaseId)
       redirect HomeR

handleReleaseCommentR :: InfoHash -> CommentId -> Handler Html
handleReleaseCommentR _ _ = redirect HomeR

{-----------------------------------------------------------------------
--  Torrent file resource
-----------------------------------------------------------------------}

instance ToContent Torrent where
  toContent = toContent . BE.encode

instance ToTypedContent Torrent where
  toTypedContent = TypedContent typeTorrent . toContent

getTorrentFileR :: InfoHash -> Handler Torrent
getTorrentFileR ih = withRelease ih (return . releaseFile . entityVal)

{-----------------------------------------------------------------------
--  Description Resource
-----------------------------------------------------------------------}

releaseDesc :: Release -> Description
releaseDesc Release {..}
  = Description releaseName (Textarea <$> releaseDescription)

updateDescription :: Description -> [Update Release]
updateDescription Description {..}
  = [ ReleaseName        =. name
    , ReleaseDescription =. unTextarea <$> description
    ]

getDescriptionHtml :: Release -> Handler Html
getDescriptionHtml release @ Release {..} = do
  form <- generateFormPost $ descriptionForm $ releaseDesc release
  defaultLayout $ descriptionPage form releaseTorrentId

getDescriptionR :: InfoHash -> Handler TypedContent
getDescriptionR ih = do
  withRelease ih $ \ (Entity _ release) -> do
    selectRep $ do
      provideRep $ getDescriptionHtml release

postDescriptionR :: InfoHash -> Handler Html
postDescriptionR ih = do
  modifyRelease ih editor $ \ (Entity releaseId release) -> do
    ((result, _), _) <- runFormPost (descriptionForm (releaseDesc release))
    withFormResult result $ \ desc' -> do
      _ <- runDB $ update releaseId (updateDescription desc')
      redirect (ReleaseR ih)

{-----------------------------------------------------------------------
--  Metadata Resource
-----------------------------------------------------------------------}

getMetadataHtml :: Torrent -> Handler Html
getMetadataHtml torrent @ Torrent { tInfoDict = InfoDict { idInfoHash = ih} } = do
  form <- generateFormPost (metadataForm torrent)
  defaultLayout $ metadataEditorPage form ih

getMetadataR :: InfoHash -> Handler TypedContent
getMetadataR ih = do
  withRelease ih $ \ (Entity _ release) -> do
    selectRep $ do
      provideRep $ getMetadataHtml (releaseFile release)

postMetadataR :: InfoHash -> Handler Html
postMetadataR ih = do
  modifyRelease ih editor $ \ (Entity releaseId Release {..}) -> do
    ((result, _), _) <- runFormPost (metadataForm releaseFile)
    withFormResult result $ \ torrent -> do
      _ <- runDB $ I.update releaseId [ReleaseFile =. torrent]
      redirect (ReleaseR ih)

{-----------------------------------------------------------------------
--  Discussion
-----------------------------------------------------------------------}

type DiscussionPresentation a = Release -> [CommentView] -> Handler a

getDiscussionHtml :: DiscussionPresentation Html
getDiscussionHtml release @ Release {..} comments = do
  let ih = idInfoHash $ tInfoDict releaseFile
  permissions <- getPermissions releaseAuthor
  form  <- generateFormPost commentForm
  defaultLayout $ discussionPage form release comments permissions

getDiscussionJson :: DiscussionPresentation Json
getDiscussionJson = undefined

getDiscussionFeed :: DiscussionPresentation Json
getDiscussionFeed = undefined

getDiscussionR :: InfoHash -> Handler TypedContent
getDiscussionR ih = do
  withRelease ih $ \ (Entity _ release) -> do
    comments <- runDB $ selectComments ih
    selectRep $ do
      provideRep $ getDiscussionHtml release comments

postDiscussionR :: InfoHash -> Handler Html
postDiscussionR ih = do
  withRelease ih $ \ _ -> do
    ((formResult, _), _) <- runFormPost commentForm
    withFormResult formResult $ \ (Textarea commentBody) -> do
      userId <- requireAuthId
      time   <- liftIO $ getCurrentTime
      liftIO $ print commentBody
      _ <- runDB $ I.insert $ Comment ih time userId commentBody
      redirect $ ReleaseR ih
