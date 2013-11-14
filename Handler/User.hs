module Handler.User
       ( getUserProfileR
       , getUserEditR
       , postUserEditR
       ) where

import Import
import Data.Text as T
import Data.Text.Lazy as TL
import Text.Markdown (markdown, def)
import Network.Gravatar



userNotExist :: T.Text -> Handler Html
userNotExist uid = do
  defaultLayout $ do
    setTitle "Unknown User"
    $(widgetFile "user/missing")

getUserProfileR :: T.Text -> Handler Html
getUserProfileR uid = do
  meuser <- runDB $ getBy $ UniqueUser uid
  defaultLayout $ do
    case meuser of
      Nothing                   -> do
        setTitle "Your Profile"
        $(widgetFile "user/missing")
      Just (Entity userId user) -> do
        let User name _ = user
        setTitle (toHtml name)
        $(widgetFile "user/profile")

{-----------------------------------------------------------------------
--  Edit user profile
-----------------------------------------------------------------------}

data UserEdit = UserEdit
  { editAbout :: Maybe Textarea
  }

userUpdates :: UserEdit -> [Update User]
userUpdates UserEdit {..}
  = maybe [] (\ (Textarea a) -> [UserAbout =. Just a]) editAbout

userEditForm :: User -> Form UserEdit
userEditForm (User uid about)
  = renderDivs $ do
      UserEdit <$> aopt textareaField "about" (Just (Textarea <$> about))

getUserEditR :: T.Text -> Handler Html
getUserEditR uid = do
  meuser <- runDB $ getBy $ UniqueUser uid
  case meuser of
    Nothing -> userNotExist uid
    Just (Entity userId user) -> do
      (formWidget, formEnctype) <- generateFormPost $ userEditForm user
      defaultLayout $ do
        $(widgetFile "user/edit")


postUserEditR :: T.Text -> Handler Html
postUserEditR uid = do
  meuser <- runDB $ getBy $ UniqueUser uid -- FIXME transaction ?
  case meuser of
    Nothing -> userNotExist uid
    Just (Entity userId user) -> do
        ((result, formWidget), formEnctype) <- runFormPost $ userEditForm user
        case result of
          FormSuccess userEdit -> do
            runDB $ update userId $ userUpdates userEdit
            redirect $ UserProfileR uid
          FormFailure _ -> userNotExist uid
          FormMissing -> notFound
