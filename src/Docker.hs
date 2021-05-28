{-# OPTIONS_GHC -Wno-deprecations #-}
module Docker where

import Data.Aeson ((.:))
import qualified Data.Aeson          as Aeson
import qualified Data.Aeson.Types as Aeson.Types
import           Network.HTTP.Simple as HTTP
import           RIO
import qualified Socket

newtype ContainerExitCode = ContainerExitCode Int
    deriving (Eq, Show)

newtype Image = Image Text
    deriving (Eq, Show)

data CreateContainerOptions
    = CreateContainerOptions
        { image :: Image
        }
        deriving (Eq, Show)

newtype ContainerId = ContainerId Text
    deriving (Eq, Show)

containerIdToText :: ContainerId -> Text
containerIdToText (ContainerId c) = c

exitCodeToInt :: ContainerExitCode -> Int
exitCodeToInt (ContainerExitCode code) = code

imageToText :: Image -> Text
imageToText (Image image) = image



createContainer :: CreateContainerOptions -> IO ContainerId
createContainer options = do
    manager <- Socket.newManager "/var/run/docker.sock"

    let image = imageToText options.image
    let body = Aeson.object
                [ ("Image", Aeson.toJSON image)
                , ("Tty", Aeson.toJSON True)
                , ("Labels", Aeson.object [("hs-ci-server", "")])
                , ("Cmd", "echo hello")
                , ("Entrypoint", Aeson.toJSON [Aeson.String "/bin/sh", "-c"])
                ]

    let req = HTTP.defaultRequest
            & HTTP.setRequestManager manager
            & HTTP.setRequestPath "/v1.40/containers/create"
            & HTTP.setRequestMethod "POST"
            & HTTP.setRequestBodyJSON body

    let parser = Aeson.withObject "create-container" $ \o -> do
        cId <- o .: "Id"
        pure $ ContainerId cId

    res <- HTTP.httpBS req
    -- Dump the response to stdout to check what we're getting back.
    parseResponse res parser


parseResponse
    :: HTTP.Response ByteString
    -> (Aeson.Value -> Aeson.Types.Parser a)
    -> IO a
parseResponse res parser = do
    let result = do
        value <- Aeson.eitherDecodeStrict (HTTP.getResponseBody res)
        Aeson.Types.parseEither parser value

    case result of
        Left e -> throwString e
        Right status -> pure status