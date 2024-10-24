module Handlers.LoggerSpec (spec) where

import Control.Monad.State (State, execState, put)
import qualified Data.Text as T
import Handlers.Logger
import Test.Hspec (Spec, it, shouldBe, shouldNotBe)

spec :: Spec
spec = do
  it "Logger write log message if level log message >=  level from Config" $ do
    let logHandle' =
          Handlers.Logger.Handle
            { Handlers.Logger.levelLogger = Debug,
              Handlers.Logger.writeLog = \text -> put text >> pure ()
            } ::
            Handlers.Logger.Handle (State T.Text)
    let logHandle1 = logHandle' {Handlers.Logger.levelLogger = Debug}
    -- it "Debug    vs  Debug" $ do
    execState (Handlers.Logger.logMessage logHandle1 Debug "New log") "Old log"
      `shouldBe` "[Debug] New log"

    -- it "Waring   vs  Debug" $ do
    execState (Handlers.Logger.logMessage logHandle1 Warning "New log") "Old log"
      `shouldBe` "[Warning] New log"
    -- it "Error    vs  Debug" $ do
    execState (Handlers.Logger.logMessage logHandle1 Error "New log") "Old log"
      `shouldBe` "[Error] New log"
    -- it "Fatal    vs  Debug" $ do
    execState (Handlers.Logger.logMessage logHandle1 Fatal "New log") "Old log"
      `shouldBe` "[Fatal] New log"
    let logHandle2 = logHandle' {Handlers.Logger.levelLogger = Warning}
    -- it "Warning  vs  Warning" $ do
    execState (Handlers.Logger.logMessage logHandle2 Warning "New log") "Old log"
      `shouldBe` "[Warning] New log"
    -- it "Error    vs  Warning" $ do
    execState (Handlers.Logger.logMessage logHandle2 Error "New log") "Old log"
      `shouldBe` "[Error] New log"
    -- it "Fatal    vs  Warning" $ do
    execState (Handlers.Logger.logMessage logHandle2 Fatal "New log") "Old log"
      `shouldBe` "[Fatal] New log"
    let logHandle3 = logHandle' {Handlers.Logger.levelLogger = Error}
    -- it "Error    vs  Error" $ do
    execState (Handlers.Logger.logMessage logHandle3 Error "New log") "Old log"
      `shouldBe` "[Error] New log"
    -- it "Fatal    vs  Error" $ do
    execState (Handlers.Logger.logMessage logHandle3 Fatal "New log") "Old log"
      `shouldBe` "[Fatal] New log"
    let logHandle4 = logHandle' {Handlers.Logger.levelLogger = Fatal}
    -- it "Fatal    vs  Fatal" $ do
    execState (Handlers.Logger.logMessage logHandle4 Fatal "New log") "Old log"
      `shouldBe` "[Fatal] New log"

  it "Logger don't write log message if level log message < level from Config" $ do
    let logHandle' =
          Handlers.Logger.Handle
            { Handlers.Logger.levelLogger = Debug,
              Handlers.Logger.writeLog = \text -> put text >> pure ()
            } ::
            Handlers.Logger.Handle (State T.Text)
    let logHandle5 = logHandle' {Handlers.Logger.levelLogger = Warning}
    -- it "Debug    vs  Warning" $ do
    execState (Handlers.Logger.logMessage logHandle5 Debug "New log") "Old log"
      `shouldNotBe` "[Debug] New log"
    let logHandle6 = logHandle' {Handlers.Logger.levelLogger = Error}
    -- it "Debug    vs  Error" $ do
    execState (Handlers.Logger.logMessage logHandle6 Debug "New log") "Old log"
      `shouldNotBe` "[Debug] New log"
    -- it "Warning  vs  Error" $ do
    execState (Handlers.Logger.logMessage logHandle6 Warning "New log") "Old log"
      `shouldNotBe` "[Warning] New log"
    let logHandle7 = logHandle' {Handlers.Logger.levelLogger = Fatal}
    -- it "Debug    vs  Fatal" $ do
    execState (Handlers.Logger.logMessage logHandle7 Debug "New log") "Old log"
      `shouldNotBe` "[Debug] New log"
    -- it "Warning  vs  Fatal" $ do
    execState (Handlers.Logger.logMessage logHandle7 Warning "New log") "Old log"
      `shouldNotBe` "[Warning] New log"
    -- it "Error    vs  Fatal" $ do
    execState (Handlers.Logger.logMessage logHandle7 Error "New log") "Old log"
      `shouldNotBe` "[Error] New log"
