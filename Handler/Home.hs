{-# LANGUAGE TupleSections
           , OverloadedStrings
  #-}
module Handler.Home
       ( getHomeR
       ) where

import Import
import Widget


getHomeR :: Handler Html
getHomeR = defaultLayout $ homePage []
