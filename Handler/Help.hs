{-# LANGUAGE TemplateHaskell #-}
module Handler.Help
       ( getHelpR
       ) where

import Import


getHelpR :: Handler Html
getHelpR = do
  defaultLayout $ do
    setTitle "Help"
    $(widgetFile "help")