{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Network.Wai.Middleware.Crowd
    ( -- * Settings
      CrowdSettings
    , defaultCrowdSettings
    , setCrowdKey
    , setCrowdRoot
    , setCrowdApprootStatic
    , setCrowdApprootGeneric
    , setCrowdManager
    , setCrowdAge
      -- * Middleware
    , mkCrowdMiddleware
      -- * Helpers
    , smartApproot
    , waiMiddlewareCrowdVersion
    ) where

import           Blaze.ByteString.Builder   (fromByteString, toByteString)
import           Data.Binary                (Binary)
import qualified Data.ByteString            as S
import           Data.Monoid                ((<>))
import qualified Data.Text                  as T
import           Data.Text.Encoding         (decodeUtf8With, encodeUtf8)
import           Data.Text.Encoding.Error   (lenientDecode)
import           Data.Version               (Version)
import           GHC.Generics               (Generic)
import           Network.HTTP.Client        (Manager, newManager)
import           Network.HTTP.Client.TLS    (tlsManagerSettings)
import           Network.HTTP.Types         (Header, status200, status303)
import           Network.Wai                (Middleware, Request, pathInfo,
                                             rawPathInfo, rawQueryString,
                                             responseBuilder, responseLBS)
import           Network.Wai.Approot
import           Network.Wai.ClientSession
import           Network.Wai.OpenId
import qualified Paths_wai_middleware_crowd as Paths

-- | Settings for creating the Crowd middleware.
--
-- To create a value, use 'defaultCrowdSettings' and then various setter
-- functions.
--
-- Since 0.1.0
data CrowdSettings = CrowdSettings
    { csGetKey     :: IO Key
    , csCrowdRoot  :: T.Text
    , csGetApproot :: IO (Request -> IO T.Text)
    , csGetManager :: IO Manager
    , csAge        :: Int
    }

-- | Set the function to get client session key for encrypting cookie data.
--
-- Default: 'getDefaultKey'
--
-- Since 0.1.0
setCrowdKey :: IO Key -> CrowdSettings -> CrowdSettings
setCrowdKey x cs = cs { csGetKey = x }

-- | Set the root of the Crowd service. This is used as an OpenID endpoint.
--
-- Default: @http://localhost:8095/openidserver@
--
-- Since 0.1.0
setCrowdRoot :: T.Text -> CrowdSettings -> CrowdSettings
setCrowdRoot x cs = cs { csCrowdRoot = x }

-- | The application root for this application.
--
-- This is used for constructing completion URLs when communicating with
-- Crowd's OpenID implementation.
--
-- Default: use the APPROOT environment variable.
--
-- Since 0.1.0
setCrowdApprootStatic :: T.Text -> CrowdSettings -> CrowdSettings
setCrowdApprootStatic x = setCrowdApprootGeneric $ return $ const $ return x

-- | More generalized version of 'setCrowdApprootStatic'.
--
-- Since 0.1.0
setCrowdApprootGeneric :: IO (Request -> IO T.Text) -> CrowdSettings -> CrowdSettings
setCrowdApprootGeneric x cs = cs { csGetApproot = x }

-- | Acquire an HTTP connection manager.
--
-- Default: get a new tls-enabled manager.
--
-- Since 0.1.0
setCrowdManager :: IO Manager -> CrowdSettings -> CrowdSettings
setCrowdManager x cs = cs { csGetManager = x }

-- | Number of seconds to keep an authentication cookie active
--
-- Default: 3600
--
-- Since 0.1.0
setCrowdAge :: Int -> CrowdSettings -> CrowdSettings
setCrowdAge x cs = cs { csAge = x }

-- | Default value for 'CrowdSettings'.
--
-- Since 0.1.0
defaultCrowdSettings :: CrowdSettings
defaultCrowdSettings = CrowdSettings
    { csGetKey = getDefaultKey
    , csCrowdRoot = "http://localhost:8095/openidserver"
    , csGetApproot = smartApproot
    , csGetManager = newManager tlsManagerSettings
    , csAge = 3600
    }

data CrowdState = CSNeedRedirect S.ByteString
                | CSLoggedIn S.ByteString
    deriving (Generic, Show)
instance Binary CrowdState

csKey :: S.ByteString
csKey = "crowd_state"

saveCrowdState :: Key -> Int -> CrowdState -> IO Header
saveCrowdState key age cs = saveCookieValue key csKey age cs

-- | Create the Crowd middleware based on the given settings.
--
-- Since 0.1.0
mkCrowdMiddleware :: CrowdSettings -> IO Middleware
mkCrowdMiddleware CrowdSettings {..} = do
    key <- csGetKey
    getApproot <- csGetApproot
    man <- csGetManager
    let prefix = csCrowdRoot <> "/users/"
    return $ \app req respond -> do
        cs <- loadCookieValue key csKey req

        case cs of
            Just (CSLoggedIn _) -> app req respond
            _ -> case pathInfo req of
                ["_crowd_middleware", "complete"] -> do
                    eres <- openIdComplete req man
                    case eres of
                        Left e -> respond $ responseLBS status200 [] "Login failed"
                        Right res ->
                            case T.stripPrefix prefix $ identifier $ oirOpLocal res of
                                Just username -> do
                                    cookie <- saveCrowdState key csAge $ CSLoggedIn $ encodeUtf8 username
                                    let dest =
                                            case cs of
                                                Just (CSNeedRedirect bs) -> bs
                                                _ -> "/"
                                    respond $ responseBuilder status303
                                        [ ("Location", dest)
                                        , cookie
                                        ]
                                        (fromByteString "Redirecting to " <> fromByteString dest)
                _ -> do
                    approot <- getApproot req
                    loc <- runResourceT $ getForwardUrl
                        (csCrowdRoot <> "/op")
                        (approot <> "/_crowd_middleware/complete")
                        Nothing
                        []
                        man
                    cookie <- saveCrowdState key csAge $ CSNeedRedirect
                            $ rawPathInfo req <> rawQueryString req
                    respond $ responseLBS status303
                        [ ("Location", encodeUtf8 loc)
                        , cookie
                        ]
                        "Logging in"
                    app req respond

-- | Current version
--
-- Since 0.1.0
waiMiddlewareCrowdVersion :: Version
waiMiddlewareCrowdVersion = Paths.version
