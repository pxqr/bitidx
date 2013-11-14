module Handler.User.Permissions
       ( Permissions (..)
       , getPermissions

       , Permission (..)
       , editor
       , commenter
       , reqPermission
       ) where

import Import
import Data.Text as T
import Yesod.Auth

data Permissions = Permissions
  { canComment :: Bool
  , canEdit    :: Bool
  }

getPermissions :: UserId -> Handler Permissions
getPermissions author = mkPermissions <$> maybeAuth
  where
    mkPermissions Nothing = Permissions
      { canComment = False
      , canEdit    = False
      }
    mkPermissions (Just (Entity userId _)) = Permissions
      { canComment = True
      , canEdit    = userId == author
      }

type Permission = (Permissions -> Bool, T.Text)

editor :: Permission
editor = (canEdit, "You need editor privileges for this action")

commenter :: Permission
commenter = (canEdit, "You need commenter privileges for this action")

reqPermission :: Permission -> UserId -> Handler a -> Handler a
reqPermission (selector, msg) author action = do
  permissions <- getPermissions author
  if selector permissions then action else permissionDenied msg
