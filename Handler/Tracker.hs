{-# LANGUAGE NamedFieldPuns #-}
module Handler.Tracker
       ( getTrackerR
       ) where

import Import
import Data.BEncode as BE
import Data.ByteString
import Network.Socket
import Network.Wai
import Yesod

import Data.Torrent.Progress
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
  { defNumWant            :: !Int
  , reannounceInterval    :: !Int
  , reannounceMinInterval :: !(Maybe Int)
  , compactPeerList       :: !Bool
  , completeIncomplete    :: !Bool
  }

instance Default TrackerSettings where
  def = TrackerSettings
    { defNumWant            = defaultNumWant
    , reannounceInterval    = 30 * 60
    , reannounceMinInterval = Nothing
    , compactPeerList       = False
    , completeIncomplete    = False
    }

class YesodTracker master where
  getSwarm    :: InfoHash -> HandlerT master IO (Maybe Swarm)
  setSwarm    :: InfoHash -> Swarm -> HandlerT master IO (Maybe Swarm)
  getSettings :: HandlerT master IO TrackerSettings

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

parseAnnounceQuery :: [(Text, Text)] -> AnnounceQuery
parseAnnounceQuery = undefined

getAnnounceQuery :: YesodTracker master => HandlerT master IO YesodAnnounceQuery
getAnnounceQuery = do
  YesodRequest {reqGetParams, reqWaiRequest} <- getRequest
  settings <- getSettings
  return $ yesodAnnounceQuery (parseAnnounceQuery reqGetParams) (remoteHost reqWaiRequest) settings

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
  , respPeers       = (if compactPeerList then CompactPeerList else PeerList) rPeers
  , respWarning     = Nothing
  }
  where
    opt True  a = Just a
    opt False _ = Nothing

getAnnounceInfo :: YesodTracker master
                => YesodAnnounceInfo -> HandlerT master IO AnnounceInfo
getAnnounceInfo resp = yesodAnnounceInfo resp <$> getSettings

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