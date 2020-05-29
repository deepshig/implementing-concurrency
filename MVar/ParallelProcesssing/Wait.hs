import Control.Concurrent

-- | make one process wait for another
main :: IO ()
main = getChar >>= \c -> do     { mVar <- newEmptyMVar
                                ; forkIO (do    { m <- takeMVar mVar
                                                ; print m})
                                ; threadDelay 2000000
                                ; putMVar mVar c}
