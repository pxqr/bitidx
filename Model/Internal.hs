{-# OPTIONS -fno-warn-orphans #-}
module Model.Internal ( ) where

import Prelude
import Control.Applicative
import Database.Persist.Sql
import Data.BEncode as BE
import Data.ByteString.Lazy as BL
import Data.Text as T
import Data.Torrent as T
import Data.Torrent.InfoHash
import Text.Read
import Yesod


instance PersistField InfoHash where
  toPersistValue   (InfoHash bs)          = toPersistValue bs
  fromPersistValue v = InfoHash <$> fromPersistValue v

instance PersistFieldSql InfoHash where
  sqlType _ = SqlBlob

instance PersistField Torrent where
  toPersistValue t = toPersistValue $ BL.toStrict $ encode t
  fromPersistValue v = do
    bs <- fromPersistValue v
    either (Left . T.pack) Right $ decode bs

instance PersistFieldSql Torrent where
  sqlType _ = SqlBlob

instance PathPiece InfoHash where
  fromPathPiece = readMaybe . T.unpack
  toPathPiece   = T.pack . show
