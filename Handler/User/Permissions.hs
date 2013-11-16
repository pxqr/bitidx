module Handler.User.Permissions
       ( Permissions (..)
       , reqPermissions
       , optPermissions

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

instance Default Permissions where
  def = Permissions False False

mkPermissions :: UserId -> Entity User -> Permissions
mkPermissions author (Entity userId _) = Permissions
  { canComment = True
  , canEdit    = userId == author
  }

reqPermissions :: UserId -> Handler Permissions
reqPermissions author = mkPermissions author <$> requireAuth

optPermissions :: UserId -> Handler Permissions
optPermissions author = maybe def (mkPermissions author) <$> maybeAuth

type Permission = (Permissions -> Bool, AppMessage)

editor :: Permission
editor    = (canEdit, MsgPermissionDeniedEdit)

commenter :: Permission
commenter = (canEdit, MsgPermissionDeniedComment)

reqPermission :: Permission -> UserId -> Handler a -> Handler a
reqPermission (selector, msg) author action = do
  permissions <- reqPermissions author
  if selector permissions then action else permissionDeniedI msg
