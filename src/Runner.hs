module Runner where

import           Core
import           RIO

import qualified Docker

data Service
    = Service
        { runBuild :: Build -> IO Build
        }

createService :: Docker.Service -> IO Service
createService docker = do
    pure Service
        { runBuild = runBuild_ docker
        }

runBuild_ :: Docker.Service -> Build -> IO Build
runBuild_ docker build = do
    newBuild <- Core.progress docker build
    case newBuild.state of
        BuildFinished _ ->
            pure newBuild
        _ -> do
            threadDelay (1 * 1000 * 1000)
            runBuild_ docker newBuild
