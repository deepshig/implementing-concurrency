-- function to read a char and print it
echo :: IO Char
echo =  getChar >>= \c ->
        putChar c >>
        return c

-- read a string and print it back
main :: IO ()
main = echo >>= \c ->   if c == '\n'
                            then return ()
                        else main
