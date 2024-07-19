module Logger (writeLog) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO 

-- helper :: Int -> Int
-- helper = succ

writeLog :: T.Text -> IO ()
writeLog = TIO.putStrLn
-- writeLog = TIO.appendFile "config/time.log" . (\x -> x <> "\n")
