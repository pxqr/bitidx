module Model where

import Prelude
import Yesod
import Data.Aeson.TH
import Data.Text (Text)
import Database.Persist.Quasi
import Data.Typeable (Typeable)
import Data.Torrent
import Data.Torrent.InfoHash
import Data.Time
import Model.Internal ()


share [mkPersist sqlOnlySettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")


$(deriveJSON id ''Release)
$(deriveJSON id ''Comment)
