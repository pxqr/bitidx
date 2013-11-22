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
  }

instance Default TrackerSettings where
  def = TrackerSettings
    { defNumWant            = defaultNumWant
    , maxNumWant            = 200
    , reannounceInterval    = 30 * 60
    , reannounceMinInterval = Nothing
    , compactPeerList       = False
    , completeIncomplete    = False
    }

setCompactFlag :: TrackerSettings -> Bool -> TrackerSettings
setCompactFlag settings flag = settings { compactPeerList = flag }

class YesodTracker master where
  getSwarm    :: InfoHash -> HandlerT master IO (Maybe Swarm)
  setSwarm    :: InfoHash -> Swarm -> HandlerT master IO (Maybe Swarm)
  getSettings :: HandlerT master IO TrackerSettings

-- | http://www.bittorrent.org/beps/bep_0023.html
lookupCompactFlag :: HandlerT master IO (Maybe Bool)
lookupCompactFlag = (parseFlag =<<) <$> lookupGetParam "compact"
  where
    parseFlag t = case T.decimal t of
      Left s -> Nothing
      Right (i, _)
        |  i == 0   -> Just False
        |  i == 1   -> Just True
        | otherwise -> Nothing

getAdvisedSettings :: YesodTracker master => HandlerT master IO TrackerSettings
getAdvisedSettings = do
  mcompact <- lookupCompactFlag
  settings <- getSettings
  return $ maybe settings (setCompactFlag settings) mcompact

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

data QueryParam
  = ParamInfoHash
  | ParamPeerId
  | ParamPort
  | ParamProgress
  | ParamIP
  | ParamNumWant
  | ParamEvent
    deriving (Show, Eq, Ord, Enum)

-- https://wiki.theory.org/BitTorrent_Tracker_Protocol#Response_Codes
data ParamParseFailure
  = Missing QueryParam -- ^ param not found in query string
  | Invalid QueryParam -- ^ param present but not valid.

missingOffset :: Int
missingOffset = 101

invalidOffset :: Int
invalidOffset = 150

paramFailureCode :: ParamParseFailure -> Int
paramFailureCode (Missing param) = missingOffset + fromEnum param
paramFailureCode (Invalid param) = invalidOffset + fromEnum param

textToPeerId :: Text -> Maybe PeerId
textToPeerId = undefined

textToPortNumber :: Text -> Maybe PortNumber
textToPortNumber = undefined

textToHostAddress :: Text -> Maybe HostAddress
textToHostAddress = undefined

textToNumWant :: Text -> Maybe Int
textToNumWant = undefined

textToEvent :: Text -> Maybe Event
textToEvent = undefined

paramName :: QueryParam -> Text
paramName ParamInfoHash = "info_hash"
paramName ParamPeerId   = "peer_id"
paramName ParamPort     = "port"

parseAnnounceQuery :: [(Text, Text)] -> Either ParamParseFailure AnnounceQuery
parseAnnounceQuery params = AnnounceQuery
    <$> reqParam ParamInfoHash textToInfoHash    params
    <*> reqParam ParamPeerId   textToPeerId      params
    <*> reqParam ParamPort     textToPortNumber  params
    <*> progress params
    <*> optParam ParamIP       textToHostAddress params
    <*> optParam ParamNumWant  textToNumWant     params
    <*> optParam ParamEvent    textToEvent       params
  where
    withError e = maybe (Left e) Right
    reqParam param p = withError (Missing param) . L.lookup (paramName param)
                   >=> withError (Invalid param) . p

    optParam param p ps
      | Just x <- L.lookup (paramName param) ps
      = pure <$> withError (Invalid param) (p x)
      | otherwise = pure Nothing

    progress = undefined
    ip       = undefined
    numwant  = undefined
    event    = undefined


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
  , respPeers       = (if compactPeerList then CompactPeerList else PeerList) rPeers
  , respWarning     = Nothing
  }
  where
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