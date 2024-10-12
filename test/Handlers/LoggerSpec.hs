module Handlers.LoggerSpec (spec) where

  
import Control.Monad.State (State, execState, put)
import qualified Data.Text as T
import Handlers.Logger
import Test.Hspec (Spec (..), it, shouldBe, shouldNotBe)

spec :: Spec
spec = do
  it "Logger write log message if level log message >=  level from Config" $ do
    let logHandle' =
          Handlers.Logger.Handle
            { Handlers.Logger.levelLogger = Debug,
              Handlers.Logger.writeLog = \text -> put text >> pure ()
            } ::
            Handlers.Logger.Handle (State T.Text)
    let logHandle = logHandle' {Handlers.Logger.levelLogger = Debug}
    -- it "Debug    vs  Debug" $ do
    execState (Handlers.Logger.logMessage logHandle Debug "New log") "Old log"
      `shouldBe` "[Debug] New log"

    -- it "Waring   vs  Debug" $ do
    execState (Handlers.Logger.logMessage logHandle Warning "New log") "Old log"
      `shouldBe` "[Warning] New log"
    -- it "Error    vs  Debug" $ do
    execState (Handlers.Logger.logMessage logHandle Error "New log") "Old log"
      `shouldBe` "[Error] New log"
    -- it "Fatal    vs  Debug" $ do
    execState (Handlers.Logger.logMessage logHandle Fatal "New log") "Old log"
      `shouldBe` "[Fatal] New log"
    let logHandle = logHandle' {Handlers.Logger.levelLogger = Warning}
    -- it "Warning  vs  Warning" $ do
    execState (Handlers.Logger.logMessage logHandle Warning "New log") "Old log"
      `shouldBe` "[Warning] New log"
    -- it "Error    vs  Warning" $ do
    execState (Handlers.Logger.logMessage logHandle Error "New log") "Old log"
      `shouldBe` "[Error] New log"
    -- it "Fatal    vs  Warning" $ do
    execState (Handlers.Logger.logMessage logHandle Fatal "New log") "Old log"
      `shouldBe` "[Fatal] New log"
    let logHandle = logHandle' {Handlers.Logger.levelLogger = Error}
    -- it "Error    vs  Error" $ do
    execState (Handlers.Logger.logMessage logHandle Error "New log") "Old log"
      `shouldBe` "[Error] New log"
    -- it "Fatal    vs  Error" $ do
    execState (Handlers.Logger.logMessage logHandle Fatal "New log") "Old log"
      `shouldBe` "[Fatal] New log"
    let logHandle = logHandle' {Handlers.Logger.levelLogger = Fatal}
    -- it "Fatal    vs  Fatal" $ do
    execState (Handlers.Logger.logMessage logHandle Fatal "New log") "Old log"
      `shouldBe` "[Fatal] New log"

  it "Logger don't write log message if level log message < level from Config" $ do
    let logHandle' =
          Handlers.Logger.Handle
            { Handlers.Logger.levelLogger = Debug,
              Handlers.Logger.writeLog = \text -> put text >> pure ()
            } ::
            Handlers.Logger.Handle (State T.Text)
    let logHandle = logHandle' {Handlers.Logger.levelLogger = Warning}
    -- it "Debug    vs  Warning" $ do
    execState (Handlers.Logger.logMessage logHandle Debug "New log") "Old log"
      `shouldNotBe` "[Debug] New log"
    let logHandle = logHandle' {Handlers.Logger.levelLogger = Error}
    -- it "Debug    vs  Error" $ do
    execState (Handlers.Logger.logMessage logHandle Debug "New log") "Old log"
      `shouldNotBe` "[Debug] New log"
    -- it "Warning  vs  Error" $ do
    execState (Handlers.Logger.logMessage logHandle Warning "New log") "Old log"
      `shouldNotBe` "[Warning] New log"
    let logHandle = logHandle' {Handlers.Logger.levelLogger = Fatal}
    -- it "Debug    vs  Fatal" $ do
    execState (Handlers.Logger.logMessage logHandle Debug "New log") "Old log"
      `shouldNotBe` "[Debug] New log"
    -- it "Warning  vs  Fatal" $ do
    execState (Handlers.Logger.logMessage logHandle Warning "New log") "Old log"
      `shouldNotBe` "[Warning] New log"
    -- it "Error    vs  Fatal" $ do
    execState (Handlers.Logger.logMessage logHandle Error "New log") "Old log"
      `shouldNotBe` "[Error] New log"
