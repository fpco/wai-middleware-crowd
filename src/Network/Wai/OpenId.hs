{-# LANGUAGE OverloadedStrings #-}
module Network.Wai.OpenId
    ( openIdComplete
    , Identifier (..)
    , oirOpLocal
    , getForwardUrl
    , runResourceT
    ) where

import Web.Authenticate.OpenId
import Network.Wai
import Network.HTTP.Client (Manager)
import Control.Exception
import Control.Monad.Trans.Resource
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Control.Arrow ((***))
import qualified Data.Text as T

openIdComplete :: Request -> Manager -> IO (Either AuthenticateException OpenIdResponse)
openIdComplete req man =
    try $ runResourceT $ authenticateClaimed params man
  where
    dec = decodeUtf8With lenientDecode
    params = map (dec *** maybe "" dec) $ queryString req
