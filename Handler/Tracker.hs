-- supported extensions:
--
--   no_peer_id - do not send peer id if no_peer_id=1 specified
--   http://www.bittorrent.org/beps/bep_0023.html
--
--   compact - compact=1 or compact=0
--   http://permalink.gmane.org/gmane.network.bit-torrent.general/4030
--
--
{-# LANGUAGE NamedFieldPuns #-}
module Handler.Tracker
       ( getTrackerR
       ) where

import Import
import Data.BEncode as BE
import Data.ByteString
import Data.List as L
import Data.Text.Read as T
import Network.Socket
import Network.Wai
import Yesod

import Data.Torrent.Progress
import Network.BitTorrent.Core.PeerId
import Network.BitTorrent.Core.PeerAddr
import Network.BitTorrent.Tracker.Message

-- TODO to bencoding package

typeBEncode :: ContentType
typeBEncode = "text/plain"

instance ToContent BValue where
  toContent = toContent . BE.encode

instance ToTypedContent BValue where
  toTypedContent = TypedContent typeBEncode . toContent

typeAnnounce :: ContentType
typeAnnounce = typeBEncode

instance ToContent AnnounceInfo where
  toContent = toContent . BE.encode

instance ToTypedContent AnnounceInfo where
  toTypedContent = toTypedContent . toBEncode

data PeerEntry = PeerEntry
  { peerAddr      :: !PeerAddr
  , peerCompleted :: !Bool
  }

type Swarm = [PeerEntry]

data BriefScrape = BriefScrape
  { scrapeComplete   :: {-# UNPACK #-} !Int
  , scrapeIncomplete :: {-# UNPACK #-} !Int
  }

swarmBriefScrape :: Swarm -> BriefScrape
swarmBriefScrape = undefined

data TrackerSettings = TrackerSettings
  { defNumWant            :: {-# UNPACK #-} !Int
  , maxNumWant            :: {-# UNPACK #-} !Int
  , reannounceInterval    :: {-# UNPACK #-} !Int
  , reannounceMinInterval :: !(Maybe Int)
  , compactPeerList       :: !Bool
  , completeIncomplete    :: !Bool
  , noPeerId              :: !Bool
  }

instance Default TrackerSettings where
  def = TrackerSettings
    { defNumWant            = defaultNumWant
    , maxNumWant            = 200 -- move to bittorrent package?
    , reannounceInterval    = 30 * 60 -- move to bittorrent package?
    , reannounceMinInterval = Nothing
    , compactPeerList       = False
    , completeIncomplete    = False
    , noPeerId              = False
    }

type Setter  a =       a -> TrackerSettings -> TrackerSettings
type MSetter a = Maybe a -> TrackerSettings ->  TrackerSettings

ifPresent :: Setter a -> MSetter a
ifPresent f m s = maybe s (`f` s) m

setCompactFlag, setNoPeerIdFlag :: Setter Bool
setCompactFlag  flag settings = settings { compactPeerList = flag }
setNoPeerIdFlag flag settings = settings { noPeerId        = flag }

class YesodTracker master where
  getSwarm    :: InfoHash -> HandlerT master IO (Maybe Swarm)
  setSwarm    :: InfoHash -> Swarm -> HandlerT master IO (Maybe Swarm)
  getSettings :: HandlerT master IO TrackerSettings

parseBoolFlag :: Text -> Maybe Bool
parseBoolFlag t = case T.decimal t of
  Left s -> Nothing
  Right (i, _)
    |  i == 0   -> Just False
    |  i == 1   -> Just True
    | otherwise -> Nothing

lookupGetFlag :: Text -> HandlerT master IO (Maybe Bool)
lookupGetFlag param = (parseBoolFlag =<<) <$> lookupGetParam param

getAdvisedSettings :: YesodTracker master => HandlerT master IO TrackerSettings
getAdvisedSettings = do
  mcompact  <- lookupGetFlag "compact"
  mnoPeerId <- lookupGetFlag "no_peer_id"
  settings  <- getSettings
  return $ ifPresent setCompactFlag  mcompact
         $ ifPresent setNoPeerIdFlag mnoPeerId settings

{-----------------------------------------------------------------------
--  Yesod announce query
-----------------------------------------------------------------------}

data Action
  = Start    { aNumWant :: !Int }
  | Regular  { aNumWant :: !Int }
  | Stop
  | Complete

action :: Int -> Maybe Event -> Action
action numWant (Just Started)   = Start numWant
action _       (Just Stopped)   = Stop
action _       (Just Completed) = Complete
action numWant  Nothing         = Regular numWant

data PeerEndpoint = PeerEndpoint
  { qInfoHash :: !InfoHash
  , qListenOn :: !PeerAddr
  , qProgress :: !Progress
  }

peerListenOn :: AnnounceQuery -> SockAddr -> PeerAddr
peerListenOn AnnounceQuery {..} = undefined

peerEndpoint ::  AnnounceQuery   -> SockAddr -> PeerEndpoint
peerEndpoint q @ AnnounceQuery {..} addr      = PeerEndpoint
  { qInfoHash = reqInfoHash
  , qListenOn = peerListenOn q addr
  , qProgress = reqProgress
  }

data YesodAnnounceQuery = YesodAnnounceQuery
  { qPeer   :: PeerEndpoint
  , qAction :: Action
  }

yesodAnnounceQuery ::  AnnounceQuery   -> SockAddr -> TrackerSettings     -> YesodAnnounceQuery
yesodAnnounceQuery q @ AnnounceQuery {..} addr        TrackerSettings {..} = YesodAnnounceQuery
  { qPeer   = peerEndpoint q addr
  , qAction = action (fromMaybe defNumWant reqNumWant) reqEvent
  }

data AnnounceFailure
  = InvalidNumWant   -- client requested more peers than allowed by tracker.
  | EventlessRequest -- Client sent an eventless request before the specified time.

getAnnounceQuery :: YesodTracker master => HandlerT master IO YesodAnnounceQuery
getAnnounceQuery = do
  YesodRequest {reqGetParams, reqWaiRequest} <- getRequest
  settings <- getSettings
  case parseAnnounceQuery reqGetParams of
    Left _ -> undefined -- TODO return error code
    Right query -> return $ yesodAnnounceQuery query (remoteHost reqWaiRequest) settings

{-----------------------------------------------------------------------
--  Yesod announce response
-----------------------------------------------------------------------}

data YesodAnnounceInfo = YesodAnnounceInfo
  { rBriefScrape ::  BriefScrape -- keep lazy
  , rPeers       :: ![PeerAddr]
  }

yesodAnnounceInfo :: YesodAnnounceInfo   -> TrackerSettings     -> AnnounceInfo
yesodAnnounceInfo    YesodAnnounceInfo {..} TrackerSettings {..} = AnnounceInfo
  { respComplete    = completeIncomplete `opt` scrapeComplete   rBriefScrape
  , respIncomplete  = completeIncomplete `opt` scrapeIncomplete rBriefScrape
  , respInterval    = reannounceInterval
  , respMinInterval = reannounceMinInterval
  , respPeers       = (if compactPeerList then CompactPeerList else PeerList)
                      ((if noPeerId then zeroPeerId else id) <$> rPeers)
  , respWarning     = Nothing
  }
  where
    zeroPeerId peerAddr = peerAddr { peerID = Nothing }

    opt True  a = Just a
    opt False _ = Nothing

getAnnounceInfo :: YesodTracker master
                => YesodAnnounceInfo -> HandlerT master IO AnnounceInfo
getAnnounceInfo resp = yesodAnnounceInfo resp <$> getAdvisedSettings

swarmToResponse :: Swarm -> YesodAnnounceInfo
swarmToResponse swarm = YesodAnnounceInfo
  { rBriefScrape = swarmBriefScrape swarm
  , rPeers       = peerAddr <$> swarm
  }

{-----------------------------------------------------------------------
--  Announce processing
-----------------------------------------------------------------------}

announcePeer :: YesodTracker master
                => YesodAnnounceQuery -> HandlerT master IO YesodAnnounceInfo
announcePeer q @ YesodAnnounceQuery {..} = do
  mswarm <- getSwarm $ qInfoHash qPeer
  return $ maybe undefined swarmToResponse mswarm

{-----------------------------------------------------------------------
--  Main handler
-----------------------------------------------------------------------}

getAnnounceR :: YesodTracker master => HandlerT master IO AnnounceInfo
getAnnounceR = getAnnounceQuery >>= announcePeer >>= getAnnounceInfo

getTrackerR :: Handler AnnounceInfo
getTrackerR = getAnnounceR

instance YesodTracker App where
  getSwarm    = undefined
  setSwarm    = undefined
  getSettings = undefined