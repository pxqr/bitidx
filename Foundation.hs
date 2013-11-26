module Foundation where

import Prelude
import Control.Applicative
import Yesod
import Yesod.Static
import Yesod.Auth
import Yesod.Auth.BrowserId
import Yesod.Auth.GoogleEmail
import Yesod.Default.Config
import Yesod.Default.Util (addStaticContentExternal)
import Yesod.Form.Nic
import Network.HTTP.Conduit (Manager)
import qualified Settings
import Settings.Development (development)
import qualified Database.Persist
import Database.Persist.Sql (SqlPersistT)
import Settings.StaticFiles
import Settings (widgetFile, Extra (..))
import qualified Settings as Settings
import Model
import Text.Jasmine (minifym)
import Text.Hamlet (hamletFile)
import System.Log.FastLogger (Logger)
import Data.ByteString.Base16 as Base16
import Blaze.ByteString.Builder.ByteString
import Data.Monoid
import Data.Text as T
import Data.Text.Encoding as T
import Data.Torrent.InfoHash
import Data.Version
import Network.Gravatar hiding (NotFound)
import Paths_bitidx

import Model.Entities


data App = App
    { settings      :: AppConfig DefaultEnv Extra
    , getStatic     :: Static -- ^ Settings for static file serving.
    , connPool      :: Database.Persist.PersistConfigPool Settings.PersistConf
    , httpManager   :: Manager
    , persistConfig :: Settings.PersistConf
    , appLogger     :: Logger
    }

mkMessage "App" "messages" "en"

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/handler
--
-- This function does three things:
--
-- * Creates the route datatype AppRoute. Every valid URL in your
--   application can be represented as a value of this type.
-- * Creates the associated type:
--       type instance Route App = AppRoute
-- * Creates the value resourcesApp which contains information on the
--   resources declared below. This is used in Handler.hs by the call to
--   mkYesodDispatch
--
-- What this function does *not* do is create a YesodSite instance for
-- App. Creating that instance requires all of the handler functions
-- for our application to be in scope. However, the handler functions
-- usually require access to the AppRoute datatype. Therefore, we
-- split these actions into two functions and place them in separate files.
mkYesodData "App" $(parseRoutesFile "config/routes")

type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

-- static copy for development and cdn for production
scripts =
  [ (js_jquery_min_js, "http://code.jquery.com/jquery-1.10.1.min.js" :: Text)
  , (js_bootstrap_js , "http://www.bootstrapcdn.com/twitter-bootstrap/2.3.2/js/bootstrap.min.js")
  ]

instance Yesod App where
    errorHandler NotFound = do
      fmap toTypedContent $ do
        defaultLayout $ do
          setTitleI MsgNotFoundPageTitle
          $(widgetFile "error/not-found")

    errorHandler other = defaultErrorHandler other

    approot = ApprootMaster $ appRoot . settings

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = fmap Just $ defaultClientSessionBackend
        (120 * 60) -- 120 minutes
        "config/client_session_key.aes"

    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage

        (title, parents) <- breadcrumbs
        mcurrentRoute    <- getCurrentRoute

        mAuth <- maybeAuth
        pc <- widgetToPageContent $ do
            $(combineStylesheets 'StaticR
                [ css_normalize_css
                , css_bootstrap_css
                , css_bootstrap_responsive_css
                ])

            mapM_ addScriptEither $
              (if development then Left . StaticR . fst else Right . snd) <$> scripts

            $(widgetFile "default-layout")

        giveUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    urlRenderOverride y (StaticR s) =
        Just $ uncurry (joinPath y (Settings.staticRoot $ settings y)) $ renderRoute s

    urlRenderOverride y (ReleaseCommentR ih cid) = Just $
         uncurry (joinPath y (appRoot (settings y))) (renderRoute (ReleaseR ih))
      <> copyByteString "#"
      <> copyByteString (T.encodeUtf8 (persistId cid))

    urlRenderOverride _ _ = Nothing

    authRoute _ = Just $ AuthR LoginR

    addStaticContent =
        addStaticContentExternal minifym genFileName Settings.staticDir (StaticR . flip StaticRoute [])
      where
        genFileName lbs
            | development = "autogen-" ++ base64md5 lbs
            | otherwise   = base64md5 lbs

    jsLoader _ = BottomOfBody

    shouldLog _ _source level =
        development || level == LevelWarn || level == LevelError

    makeLogger = return . appLogger

instance YesodPersist App where
    type YesodPersistBackend App = SqlPersistT
    runDB = defaultRunDB persistConfig connPool

instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner connPool

instance YesodAuth App where
    type AuthId App = UserId

    loginDest  _ = HomeR
    logoutDest _ = HomeR

    getAuthId creds = runDB $ do
        x <- getBy $ UniqueUser $ credsIdent creds
        case x of
            Just (Entity uid _) -> return $ Just uid
            Nothing -> do
                fmap Just $ insert $ User (credsIdent creds) Nothing

    authPlugins _ = [ authGoogleEmail]
    authHttpManager = httpManager

{-----------------------------------------------------------------------
--  Current route check
--
--  predicates used to style current active page in navbar
-----------------------------------------------------------------------}
checkMaybe :: Maybe a -> (a -> Bool) -> Bool
checkMaybe = flip (maybe False)

isSearchRoute :: Route App -> Bool
isSearchRoute = (==) SearchR

isAddRoute :: Route App -> Bool
isAddRoute r = r == AddR

isUserRoute :: Route App -> Bool
isUserRoute (UserProfileR _) = True
isUserRoute (UserEditR    _) = True
isUserRoute  _               = False

isLoginRoute :: Route App -> Bool
isLoginRoute (AuthR r) = r == LoginR
isLoginRoute _         = False

isSearchableRoute :: Route App -> Bool
isSearchableRoute = undefined

instance YesodBreadcrumbs App where
  breadcrumb  HomeR             = return ("Home"       , Nothing)
  breadcrumb (SearchR        )  = return ("Search"     , Just HomeR)
  breadcrumb (AddR           )  = return ("Add"        , Just HomeR)
  breadcrumb (ReleaseR     ih)  = return (longHex ih   , Just HomeR)
  breadcrumb (DescriptionR ih)  = return ("Description", Just (ReleaseR ih))
  breadcrumb (MetadataR    ih)  = return ("Metadata"   , Just (ReleaseR ih))
  breadcrumb (DiscussionR  ih)  = return ("Discussion" , Just (ReleaseR ih))
  breadcrumb (UserProfileR uid) = return (uid          , Just HomeR)
  breadcrumb (UserEditR    uid) = return ("Edit"       , Just (UserProfileR uid))
  breadcrumb _                  = return (""           , Just HomeR)

instance YesodNic App where

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

getExtra :: Handler Extra
getExtra = fmap (appExtra . settings) getYesod
