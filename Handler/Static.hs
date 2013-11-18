module Handler.Static
       ( getHomeR
       , getHelpR
       ) where

import Import
import Widget


getHomeR :: Handler Html
getHomeR = defaultLayout $ homePage []

getHelpR :: Handler Html
getHelpR = defaultLayout helpW
