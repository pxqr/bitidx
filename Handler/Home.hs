{-# LANGUAGE TupleSections
           , OverloadedStrings
  #-}
module Handler.Home
       ( getHomeR
       ) where

import Import
import Widget


getNewTorrents :: Handler [Release]
getNewTorrents = do
  newRevisionsEnts <- runDB $ selectList [] [LimitTo 10]
  return $ fmap entityVal newRevisionsEnts

getHomeR :: Handler Html
getHomeR = do
  releases <- getNewTorrents
  defaultLayout $ homePage releases
