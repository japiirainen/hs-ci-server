module Main where

import           Core
import qualified Docker
import qualified Runner
import           RIO
import qualified RIO.Map              as Map
import qualified RIO.NonEmpty.Partial as NonEmpty.Partial
import qualified System.Process.Typed as Process
import           Test.Hspec

makeStep :: Text -> Text -> [Text] -> Step
makeStep name image commands
 = Step
 { name = StepName name
 , image = Docker.Image image
 , commands = NonEmpty.Partial.fromList commands }


makePipeline :: [Step] -> Pipeline
makePipeline steps = Pipeline { steps  = NonEmpty.Partial.fromList steps }

runBuild :: Docker.Service -> Build -> IO Build
runBuild docker build = do
    newBuild <- Core.progress docker build
    case newBuild.state of
        BuildFinished _ ->
            pure newBuild
        _ -> do
            threadDelay (1 * 1000 * 1000)
            runBuild docker newBuild

testRunSuccess :: Runner.Service -> IO ()
testRunSuccess runner = do
    build <- runner.prepareBuild $ makePipeline
                [ makeStep "First step" "ubuntu" ["date"]
                , makeStep "Second step" "ubuntu" ["uname -r"]
                ]
    result <- runner.runBuild build

    result.state `shouldBe` BuildFinished BuildSucceeded
    Map.elems result.completedSteps `shouldBe` [StepSucceeded, StepSucceeded]

main :: IO ()
main = hspec do
    docker <- runIO Docker.createService
    runner <- runIO $ Runner.createService docker

    beforeAll cleanupDocker $ describe "CI" do
        it "should run a build (success)" do
            testRunSuccess runner

cleanupDocker :: IO ()
cleanupDocker = void do
    Process.readProcessStdout "docker rm -f $(docker ps -aq --filter \"label=hs-ci-server\")"
