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
import Widget

withUser :: T.Text -> (Entity User -> Handler a) -> Handler a
withUser userName action = do
  meuser <- runDB $ getBy $ UniqueUser userName
  maybe notFound action meuser

getUserProfileR :: T.Text -> Handler Html
getUserProfileR userName = do
  withUser userName $ \ (Entity userId user) -> do
    defaultLayout $ userProfilePage user

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
userEditForm (User uid about) = renderDivs $ do
  UserEdit <$> aopt textareaField "about" (Just (Textarea <$> about))

getUserEditR :: T.Text -> Handler Html
getUserEditR userName = do
  withUser userName $ \ (Entity userId user) -> do
    form <- generateFormPost $ userEditForm user
    defaultLayout $ userEditPage form user

postUserEditR :: T.Text -> Handler Html
postUserEditR userName = do
  withUser userName $ \ (Entity userId user) -> do
    ((result, formWidget), formEnctype) <- runFormPost $ userEditForm user
    withFormResult result $ \ userEdit -> do
      runDB $ update userId $ userUpdates userEdit
      redirect $ UserProfileR userName
