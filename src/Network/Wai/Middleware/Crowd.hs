{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
import GHC.Generics (Generic)
import Blaze.ByteString.Builder (toByteString, fromByteString)
import Network.Wai
import Control.Exception
import Network.Wai.Handler.Warp (run)
import Data.Monoid
import Network.HTTP.Types
import Web.ClientSession
import qualified Data.ByteString as S
import System.Environment
import Data.Text.Encoding (encodeUtf8, decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Web.Authenticate.OpenId
import Network.HTTP.Client (newManager, Manager)
import Network.HTTP.Client.TLS
import Control.Monad.Trans.Resource
import qualified Data.Text as T
import Data.Binary
import Web.Cookie
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Base64.URL as B64
import Network.Wai.Application.Static

app :: Application
app _ respond = respond $ responseLBS status200 [] "I'm a teapot!"

main :: IO ()
main = do
    crowdMiddleware <- mkCrowdMiddleware defaultCrowdSettings
    run 3000 $ crowdMiddleware $ staticApp $ defaultFileServerSettings "."

data CrowdSettings = CrowdSettings
    { csGetKey :: IO Key
    , csCrowdRoot :: T.Text
    , csGetApproot :: IO T.Text
    , csGetManager :: IO Manager
    }

defaultCrowdSettings :: CrowdSettings
defaultCrowdSettings = CrowdSettings
    { csGetKey = getDefaultKey
    , csCrowdRoot = "http://localhost:8095/openidserver"
    , csGetApproot = fmap T.pack $ getEnv "APPROOT"
    , csGetManager = newManager tlsManagerSettings
    }

data CrowdState = CSNeedRedirect S.ByteString
                | CSLoggedIn S.ByteString
                | CSNoState
    deriving (Generic, Show)
instance Binary CrowdState

csKey = "crowd_state"

loadCrowdState key req =
    case lookup csKey cookies >>= decrypt key . B64.decodeLenient of
        Just val | Right (_, _, res) <- decodeOrFail (L.fromStrict val) -> return res
        _ -> return CSNoState
  where
    cookies = concat [parseCookies v | (k, v) <- requestHeaders req, k == "cookie"]

saveCrowdState key cs = do
    val <- encryptIO key $ L.toStrict $ encode cs
    return $ toByteString $ renderSetCookie def
        { setCookieName = csKey
        , setCookieValue = B64.encode val
        , setCookiePath = Just "/"
        , setCookieMaxAge = Just 3600
        , setCookieHttpOnly = True
        }

mkCrowdMiddleware :: CrowdSettings -> IO Middleware
mkCrowdMiddleware CrowdSettings {..} = do
    key <- csGetKey
    approot <- csGetApproot
    man <- csGetManager
    let prefix = csCrowdRoot <> "/users/"
    return $ \app req respond -> do
        cs <- loadCrowdState key req
        print (cs, pathInfo req)
        case cs of
            CSLoggedIn _ -> app req respond
            _ -> case pathInfo req of
                ["_crowd_middleware", "complete"] -> do
                    let dec = decodeUtf8With lenientDecode
                        params =
                            [ (dec k, dec v)
                            | (k, Just v) <- queryString req
                            ]
                    eres <- try $ runResourceT $ authenticateClaimed params man
                    case eres :: Either AuthenticateException OpenIdResponse of
                        Left e -> respond $ responseLBS status200 [] "Login failed"
                        Right res ->
                            case T.stripPrefix prefix $ identifier $ oirOpLocal res of
                                Just username -> do
                                    print $ CSLoggedIn $ encodeUtf8 username
                                    cookie <- saveCrowdState key $ CSLoggedIn $ encodeUtf8 username
                                    let dest =
                                            case cs of
                                                CSNeedRedirect bs -> bs
                                                _ -> "/"
                                    print cookie
                                    respond $ responseBuilder status200
                                        [ ("Location", dest)
                                        , ("Set-Cookie", cookie)
                                        ]
                                        (fromByteString "Redirecting to " <> fromByteString dest)
                _ -> do
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
                        , ("Set-Cookie", cookie)
                        ]
                        "Logging in"
                    app req respond

