import Control.Concurrent

-- | function to read a char and print it
echo :: IO Char
echo =  getChar >>= \c ->
        putChar c >>
        return c

-- | recursive function to take a character and print it
loop :: Char -> IO ()
loop ch = putChar ch >> loop ch

-- | recursive function to take a character and print it
loopWithDelay :: Char -> IO ()
loopWithDelay ch = do   { threadDelay 50000
                        ; putChar ch
                        ; loopWithDelay ch}


-- | run 2 process in parallel
main :: IO ()
main = forkIO (loopWithDelay '\n') >>
        (loop 'z')
