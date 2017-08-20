-- This is copied from https://github.com/yesodweb/yesod/tree/master/yesod-bin/devel-example
module Bloggo
    ( prodMain
    , develMain
    ) where

import Bloggo.Machinery         (runAppOn)
import Control.Concurrent       (threadDelay)
import Control.Concurrent.Async (race_)
import System.Directory         (doesFileExist)
import System.Environment

prodMain :: IO ()
prodMain = runAppOn 8080 Nothing

develMain :: IO ()
develMain = race_ watchTermFile $ do
    port <- fmap read $ getEnv "PORT"
    displayPort <- getEnv "DISPLAY_PORT"
    putStrLn $ "Running in development mode on port " ++ show port
    putStrLn $ "But you should connect to port " ++ displayPort
    runAppOn port Nothing

-- | Would certainly be more efficient to use fsnotify, but this is
-- simpler.
watchTermFile :: IO ()
watchTermFile =
    loop
  where
    loop = do
        exists <- doesFileExist "yesod-devel/devel-terminate"
        if exists
            then return ()
            else do
                threadDelay 100000
                loop
