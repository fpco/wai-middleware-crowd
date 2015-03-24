{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
import           Data.String                    (fromString)
import qualified Data.Text                      as T
import           Data.Version                   (showVersion)
import           Development.GitRev             (gitDirty, gitHash)
import           Network.HTTP.Client            (Manager, newManager)
import           Network.HTTP.Client.TLS        (tlsManagerSettings)
import           Network.HTTP.ReverseProxy      (ProxyDest (..), WaiProxyResponse (WPRProxyDest),
                                                 defaultOnExc, waiProxyTo)
import           Network.Wai                    (Application)
import           Network.Wai.Application.Static (defaultFileServerSettings,
                                                 staticApp)
import           Network.Wai.Handler.Warp       (run)
import           Network.Wai.Middleware.Crowd
import           Options.Applicative
import           Web.ClientSession              (getKey)

versionString :: String
versionString = concat
    [ "Version "
    , showVersion waiMiddlewareCrowdVersion
    , ", Git revision "
    , $gitHash
    , if $gitDirty then " (dirty)" else ""
    ]

data BasicSettings = BasicSettings
    { warpPort  :: Int
    , keyFile   :: FilePath
    , crowdRoot :: T.Text
    }
    deriving Show

basicSettingsParser :: Parser BasicSettings
basicSettingsParser = BasicSettings
    <$> option auto
        ( long "listen-port"
       <> short 'p'
       <> metavar "LISTEN-PORT"
       <> help "Port to listen on for requests"
       <> value 3000 )
    <*> strOption
        ( long "key-file"
       <> short 'k'
       <> metavar "KEY-FILE"
       <> help "File containing the clientsession key"
       <> value "" )
    <*> (T.pack <$> strOption
        ( long "crowd-root"
       <> metavar "CROWD-ROOT"
       <> help "Base URL for the Crowd installation"
       <> value "" ))

opts :: ParserInfo (BasicSettings, Service)
opts =
    info
        (helpOption <*> versionOption <*> config)
        ( fullDesc
       <> progDesc "Run a Crowd-authenticated file server or reverse proxy"
       <> header "wai-crowd - a Crowd-authenticated server" )
  where
    helpOption =
        abortOption ShowHelpText $
        long "help" <>
        help "Show this help text"
    versionOption =
        infoOption
            versionString
            (long "version" <>
             help "Show version")
    config = (,) <$> basicSettingsParser <*> serviceParser

data Service = ServiceFiles FileServer
             | ServiceProxy ReverseProxy

serviceParser :: Parser Service
serviceParser = subparser $
    ( command "file-server"
        (info (ServiceFiles <$> fileServerParser) (progDesc "File server")) ) <>
    ( command "reverse-proxy"
        (info (ServiceProxy <$> reverseProxyParser) (progDesc "Reverse proxy")))

data FileServer = FileServer
    { fsRoot :: FilePath
    }

fileServerParser = FileServer
    <$> (argument str
         (metavar "ROOT-DIR" <> value "."))

data ReverseProxy = ReverseProxy
    { rpHost :: String
    , rpPort :: Int
    }

reverseProxyParser :: Parser ReverseProxy
reverseProxyParser = ReverseProxy
    <$> (argument str (metavar "HOST"))
    <*> (argument auto (metavar "PORT"))

serviceToApp :: Manager -> Service -> IO Application
serviceToApp _ (ServiceFiles FileServer {..}) =
    return $ staticApp $ defaultFileServerSettings $ fromString fsRoot
serviceToApp manager (ServiceProxy (ReverseProxy host port)) =
    return $ waiProxyTo
        (const $ return $ WPRProxyDest $ ProxyDest (fromString host) port)
        defaultOnExc
        manager

main :: IO ()
main = do
    (BasicSettings {..}, service) <- execParser opts
    manager <- newManager tlsManagerSettings
    let cs = (if null keyFile then id else setCrowdKey (getKey keyFile))
           $ (if T.null crowdRoot then id else setCrowdRoot crowdRoot)
           $ setCrowdManager (return manager)
           $ defaultCrowdSettings
    crowdMiddleware <- mkCrowdMiddleware cs
    app <- serviceToApp manager service
    run warpPort $ crowdMiddleware app
