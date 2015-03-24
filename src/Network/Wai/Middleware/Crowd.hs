{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
module Network.Wai.Middleware.Client
    ( -- * Settings
      CrowdSettings
    , defaultCrowdSettings
    , setCrowdKey
    , setCrowdRoot
    , setCrowdApprootStatic
    , setCrowdApprootGeneric
    , setCrowdManager
      -- * Middleware
    , mkCrowdMiddleware
    ) where

import Network.Wai.ClientSession
import GHC.Generics (Generic)
import Blaze.ByteString.Builder (toByteString, fromByteString)
import Network.Wai
import Control.Exception
import Data.Monoid
import Network.HTTP.Types
import qualified Data.ByteString as S
import System.Environment
import Data.Text.Encoding (encodeUtf8, decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Network.HTTP.Client (newManager, Manager)
import Network.HTTP.Client.TLS
import Control.Monad.Trans.Resource
import qualified Data.Text as T
import Data.Binary
import Network.Wai.OpenId

-- | Settings for creating the Crowd middleware.
--
-- To create a value, use 'defaultCrowdSettings' and then various setter
-- functions.
--
-- Since 0.1.0
data CrowdSettings = CrowdSettings
    { csGetKey :: IO Key
    , csCrowdRoot :: T.Text
    , csGetApproot :: IO (Request -> IO T.Text)
    , csGetManager :: IO Manager
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

defaultCrowdSettings :: CrowdSettings
defaultCrowdSettings = CrowdSettings
    { csGetKey = getDefaultKey
    , csCrowdRoot = "http://localhost:8095/openidserver"
    , csGetApproot = do
        ar <- fmap T.pack $ getEnv "APPROOT"
        return $ const $ return ar
    , csGetManager = newManager tlsManagerSettings
    }

data CrowdState = CSNeedRedirect S.ByteString
                | CSLoggedIn S.ByteString
                | CSNoState
    deriving (Generic, Show)
instance Binary CrowdState

csKey = "crowd_state"

saveCrowdState key cs = saveCookieValue key csKey 3600 cs

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
                                    print $ CSLoggedIn $ encodeUtf8 username
                                    cookie <- saveCrowdState key $ CSLoggedIn $ encodeUtf8 username
                                    let dest =
                                            case cs of
                                                Just (CSNeedRedirect bs) -> bs
                                                _ -> "/"
                                    print cookie
                                    respond $ responseBuilder status200
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
                    cookie <- saveCrowdState key $ CSNeedRedirect
                            $ rawPathInfo req <> rawQueryString req
                    respond $ responseLBS status303
                        [ ("Location", encodeUtf8 loc)
                        , cookie
                        ]
                        "Logging in"
                    app req respond
