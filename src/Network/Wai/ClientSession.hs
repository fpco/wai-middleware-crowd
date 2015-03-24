{-# LANGUAGE OverloadedStrings #-}
module Network.Wai.ClientSession
    ( loadCookieValue
    , saveCookieValue
    , Key
    , getDefaultKey
    ) where

import Blaze.ByteString.Builder (toByteString)
import Web.ClientSession
import Network.Wai
import Web.Cookie
import Data.Binary
import qualified Data.ByteString.Base64.URL as B64
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as S
import Data.CaseInsensitive (CI)
import Data.Maybe (listToMaybe)
import Control.Monad (guard)
import Network.HTTP.Types
import Data.Time (DiffTime)

loadCookieValue :: Binary value
                => Key
                -> S.ByteString -- ^ cookie name
                -> Request
                -> IO (Maybe value)
loadCookieValue key name req = return $ listToMaybe $ do
    (k, v) <- requestHeaders req
    guard $ k == "cookie"
    (name', v') <- parseCookies v
    guard $ name == name'
    Right v'' <- return $ B64.decode v'
    Right (_, _, res) <- return $ decodeOrFail $ L.fromStrict v''
    return res

saveCookieValue :: Binary value
                => Key
                -> S.ByteString -- ^ cookie name
                -> DiffTime -- ^ age
                -> value
                -> IO Header
saveCookieValue key name age value = do
    value' <- encryptIO key $ L.toStrict $ encode value
    return ("Set-Cookie", toByteString $ renderSetCookie def
        { setCookieName = name
        , setCookieValue = B64.encode value'
        , setCookiePath = Just "/"
        , setCookieHttpOnly = True
        , setCookieMaxAge = Just age
        })
