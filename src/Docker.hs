module Docker where

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

createContainer :: CreateContainerOptions -> IO ()
createContainer options = undefined

exitCodeToInt :: ContainerExitCode -> Int
exitCodeToInt (ContainerExitCode code) = code

imageToText :: Image -> Text
imageToText (Image image) = image
