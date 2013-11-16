module Handler.Release.Recent
       ( Recent
       , getRecentlyViewed
       , addRecentlyViewed
       ) where

import Import
import Data.BEncode as BE
import Data.ByteString.Lazy as BL
import Data.List as L


-- | Queue used for recently viewed releases list.
type Recent = [InfoHash]

-- TODO to settings (from cookies?)
maxQueueLen :: Int
maxQueueLen = 10

addRecent :: InfoHash -> Recent -> Recent
addRecent ih r = L.take maxQueueLen $ ih : L.delete ih r

recentKey :: Text
recentKey = "recently-viewed"

getRecentlyViewed :: Handler Recent
getRecentlyViewed = do
  mbs <- lookupSessionBS recentKey
  return $ maybe def (either (const def) id . decode) mbs

setRecentlyViewed :: Recent -> Handler ()
setRecentlyViewed = setSessionBS recentKey . BL.toStrict . encode

addRecentlyViewed :: InfoHash -> Handler ()
addRecentlyViewed ih = getRecentlyViewed >>= setRecentlyViewed . addRecent ih
