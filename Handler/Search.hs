{-# LANGUAGE TemplateHaskell
           , NamedFieldPuns
   #-}
module Handler.Search
       ( getSearchR
       ) where

import Import
import Data.List as L
import Data.Text as T
import Database.Esqueleto as S
import Widget


getSearchString :: Handler (Maybe Text)
getSearchString = do
  mq <- lookupGetParam searchQueryParamName
  case mq of
    Nothing -> return Nothing
    Just q
      | T.null q  -> return Nothing
      | otherwise -> return (Just q)

searchReleasesT :: Text -> YesodDB App [Entity Release]
searchReleasesT qstr =
  S.select $
  S.from $ \ release -> do
    let pattern = "%" <> qstr <> "%"
    S.where_  (release ^. ReleaseName `like` val pattern)
    return release

getSearchJson :: [Release] -> Handler Json
getSearchJson releases = return $ toJSON releases

getSearchHtml :: Maybe Text -> [Release] -> Handler Html
getSearchHtml msearchString releases = defaultLayout (searchPage msearchString releases)

-- TODO feed for search string

getSearchR :: Handler TypedContent
getSearchR = do
  msearchString <- getSearchString
  releases      <- fmap entityVal <$> do
    maybe (pure []) (runDB . searchReleasesT) msearchString

  selectRep $ do
    provideRep $ getSearchHtml msearchString releases
    provideRep $ getSearchJson               releases
