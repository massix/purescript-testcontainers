module Test.WaitStrategy (waitStrategyTest) where

import Prelude

import Data.DateTime.Instant (unInstant)
import Data.Either (Either(..), isRight)
import Data.String.Utils (includes)
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (error, throwError)
import Effect.Class (liftEffect)
import Effect.Now (now)
import Partial.Unsafe (unsafePartial)
import Test.Assertions (shouldInclude)
import Test.Partials (forceRight)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Test.TestContainers (exec, setCommand, setExposedPorts, setStartupTimeout, setWaitStrategy, withContainer)
import Test.TestContainers.Monad (setCommandM, setEnvironmentM, setStartupTimeoutM, setWaitStrategyM)
import Test.TestContainers.Types (StartupTimeout(..), WaitStrategy(..))
import Test.Utils (mkAffContainer, mkAffContainerM)

waitStrategyTest :: Spec Unit
waitStrategyTest = describe "Wait Strategies" $ do

  describe "Log Based" $ do
    it "should wait for a line of log to appear twice" $ do
      psql <- mkAffContainerM "postgres:14-alpine" $ do
        setEnvironmentM psqlEnv

        -- while instanciating, the psql container will start twice, one to setup the basic
        -- environment, and the second one with the custom configuration, so we have to
        -- wait for the same line of log to appear twice!
        setWaitStrategyM [ LogOutput "ready to accept connections" 2 ]

      res <- withContainer psql $ \c -> do
        execResult <- exec [ "psql", "-U", "test", "-c", "SELECT true" ] c
        execResult `shouldSatisfy` isRight

        let { output } = unsafePartial $ forceRight execResult
        output `shouldInclude` "(1 row)"

      case res of
        Left e -> throwError $ error e
        Right _ -> pure unit

    it "should wait for a certain amount of time" $ do
      rightNow <- liftEffect now
      sleeper <- mkAffContainerM "alpine:latest" $ do
        setCommandM [ "sleep", "360" ]
        setStartupTimeoutM $ StartupTimeout 1000

        -- Wait 6 seconds
        setWaitStrategyM [ LogOutput "this should never arrive" 10 ]

      -- We don't care about the container here since it will fail to start
      res <- withContainer sleeper $ \_ -> do
        pure unit

      case res of
        -- We expect an error here
        Left e -> do
          afterError <- liftEffect now
          compareInstants (unInstant rightNow) (unInstant afterError) `shouldSatisfy` (flip (>) 1000.0)
          e `shouldInclude` "not received after 1000ms"
        Right _ -> throwError $ error "I was expecting an error!"

  describe "HTTP Based" $ do
    it "should wait for an http code" $ do
      rightNow <- liftEffect now
      nginx <- mkAffContainer "nginx:latest" $
        -- For HttpStatusCode to work we have to expose that port
        setWaitStrategy [ HttpStatusCode "/" 80 200 ]
          <<< setExposedPorts [ 80 ]
          <<< setStartupTimeout (StartupTimeout 3000)

      res <- withContainer nginx $ \_ -> do
        afterStartup <- liftEffect now
        compareInstants (unInstant rightNow) (unInstant afterStartup) `shouldSatisfy` (flip (>) 1000.0)

      case res of
        Left e -> throwError $ error e
        Right _ -> pure unit

    it "should parse the body of an http request" $ do
      rightNow <- liftEffect now
      nginx <- mkAffContainer "nginx:latest" $
        setWaitStrategy [ HttpResponsePredicate "/" 80 checkHttpBody ]
          <<< setExposedPorts [ 80 ]
          <<< setStartupTimeout (StartupTimeout 3000)

      res <- withContainer nginx $ \_ -> do
        afterStartup <- liftEffect now
        compareInstants (unInstant rightNow) (unInstant afterStartup) `shouldSatisfy` (flip (>) 1000.0)

      case res of
        Left e -> throwError $ error e
        Right _ -> pure unit

  describe "Shell commands" $ do
    it "should wait for a shell command output" $ do
      rightNow <- liftEffect now
      sleeper <- mkAffContainer "alpine:latest" $
        setWaitStrategy [ ShellCommand "sleep 3" ]
          <<< setCommand [ "sleep", "30" ]
          <<< setStartupTimeout (StartupTimeout 15000)

      res <- withContainer sleeper $ \_ -> do
        afterStartup <- liftEffect now
        compareInstants (unInstant rightNow) (unInstant afterStartup) `shouldSatisfy` (flip (>) 3000.0)

      case res of
        Left e -> throwError $ error e
        Right _ -> pure unit

    it "should fail if shell command exit code != 0" $ do
      failer <- mkAffContainer "alpine:latest" $
        setWaitStrategy [ ShellCommand "sleep 1; false" ]
          <<< setCommand [ "sleep", "30" ]
          <<< setStartupTimeout (StartupTimeout 4000)

      res <- withContainer failer $ \_ -> do
        throwError $ error "should never arrive here!"

      case res of
        Left e -> e `shouldEqual` "Shell command \"sleep 1; false\" not successful after 4000ms"
        Right _ -> throwError $ error "I was expecting an error!"

  where
  psqlEnv =
    [ { key: "POSTGRES_USER", value: "test" }
    , { key: "POSTGRES_PASSWORD", value: "test" }
    , { key: "POSTGRES_DB", value: "test" }
    ]

  compareInstants :: Milliseconds -> Milliseconds -> Number
  compareInstants (Milliseconds a) (Milliseconds b) = b - a

  checkHttpBody :: String -> Boolean
  checkHttpBody b =
    "Welcome to nginx!" `includes` b
