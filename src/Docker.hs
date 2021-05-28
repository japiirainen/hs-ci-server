{-# OPTIONS_GHC -Wno-deprecations #-}
module Docker where

import           Network.HTTP.Simple as HTTP
import           RIO

newtype ContainerExitCode = ContainerExitCode Int
    deriving (Eq, Show)

newtype Image = Image Text
    deriving (Eq, Show)

data CreateContainerOptions
    = CreateContainerOptions
        { image :: Image
        }
        deriving (Eq, Show)

exitCodeToInt :: ContainerExitCode -> Int
exitCodeToInt (ContainerExitCode code) = code

imageToText :: Image -> Text
imageToText (Image image) = image



createContainer :: CreateContainerOptions -> IO ()
createContainer options = do
    let body = ()
    let req = HTTP.defaultRequest
            & HTTP.setRequestPath "/v1.40/containers/create"
            & HTTP.setRequestMethod "POST"
            & HTTP.setRequestBodyJSON body
    res <- HTTP.httpBS req

    -- Dump the response to stdout to check what we're getting back.
    traceShowIO res
