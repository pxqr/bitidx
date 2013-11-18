{-# LANGUAGE TemplateHaskell #-}
module Handler.Help
       ( getHelpR
       ) where

import Import
import Widget


getHelpR :: Handler Html
getHelpR = defaultLayout helpW
