{-# LANGUAGE OverloadedStrings #-}
-- | rdr2tls as a middleware
module Rdr2tls (rdr2tls) where

import Network.HTTP.Types (status307)
import Network.Wai
import Network.Wai.Request
import qualified Data.ByteString as S

rdr2tls :: Middleware
rdr2tls app req send
    | appearsSecure req = app req send
    | otherwise =
        let dest = S.concat
                [ guessApproot req { isSecure = True }
                , rawPathInfo req
                , rawQueryString req
                ]
         in send $ responseLBS status307 [("Location", dest)] "Redirecting"
