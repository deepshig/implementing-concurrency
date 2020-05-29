-- get a char from stdin and print it out twice
main :: IO ()
main = (getChar) >>= \c -> (putChar c) >> (putChar c)
