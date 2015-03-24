{-# LANGUAGE OverloadedStrings #-}
module Network.Wai.OpenId
    ( openIdComplete
    , Identifier (..)
    , oirOpLocal
    , getForwardUrl
    , runResourceT
    ) where

import           Control.Arrow                ((***))
import           Control.Exception            (try)
import           Control.Monad.Trans.Resource (runResourceT)
import qualified Data.Text                    as T
import           Data.Text.Encoding           (decodeUtf8With)
import           Data.Text.Encoding.Error     (lenientDecode)
import           Network.HTTP.Client          (Manager)
import           Network.Wai                  (Request, queryString)
import           Web.Authenticate.OpenId      (AuthenticateException (..),
                                               Identifier (..), OpenIdResponse,
                                               authenticateClaimed,
                                               getForwardUrl, oirOpLocal)

openIdComplete :: Request -> Manager -> IO (Either AuthenticateException OpenIdResponse)
openIdComplete req man =
    try $ runResourceT $ authenticateClaimed params man
  where
    dec = decodeUtf8With lenientDecode
    params = map (dec *** maybe "" dec) $ queryString req
