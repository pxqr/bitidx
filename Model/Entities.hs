module Model.Entities
       ( LastUpdated (..)
       , PersistId (..)

       , formatDatetime
       , formatEventTime
       ) where

import Import
import Data.Text as T
import Data.Time
import System.Locale


class LastUpdated a where
  lastUpdated :: a -> UTCTime

instance LastUpdated a => LastUpdated (Entity a) where
  lastUpdated = lastUpdated . entityVal

instance LastUpdated Comment where
  lastUpdated (Comment _ updated _ _) = updated

instance LastUpdated Release where
  lastUpdated (Release _ _ _ _ _ uploaded) = uploaded

class PersistId a where
  persistId :: Key a -> Text

instance PersistId Comment where
  persistId = either T.pack ("comment-" <>) . fromPersistValueText . unKey

formatDatetime :: UTCTime -> String
formatDatetime = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%z"

formatEventTime :: UTCTime -> String
formatEventTime = formatTime defaultTimeLocale "%B %e, %Y at %T"
