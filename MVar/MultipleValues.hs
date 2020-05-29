import Control.Concurrent

-- function to read a char and print it
echo :: IO Char
echo =  getChar >>= \c ->
        putChar c >>
        return c

-- push twice in Mvar before consumption happens of the first value -> second value goes into mvar only after first is consumed
main :: IO ()
main = echo >>= \c -> do
                            mVar <- newEmptyMVar
                            forkIO (do  threadDelay 4000000
                                        m <- takeMVar mVar
                                        print m)
                                >> putMVar mVar 'f'
                                >> print "first value pushed"
                                >> putMVar mVar c
                                >> print "second value pushed"
