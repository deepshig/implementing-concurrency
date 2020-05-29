import Control.Concurrent

-- dataVar : Producer -> consumer
-- ackVar : Consumer -> producer
type CVar a = (MVar a, MVar ())

newCVar :: IO (CVar a)
newCVar = newEmptyMVar >>= \dataVar ->
              newEmptyMVar >>= \ackVar ->
                  putMVar ackVar () >>
                  return (dataVar, ackVar)

putCVar :: CVar a -> a -> IO ()
putCVar (dataVar, ackVar) val =
    print "consumed ackVar" >>
    takeMVar ackVar >>
    print "publishing dataVar" >>
    putMVar dataVar val

takeCVar :: CVar a -> IO a
takeCVar (dataVar, ackVar) =
    print "consumed dataVar" >>
    takeMVar dataVar >>= \val ->
        print "publishing ackVar" >>
        putMVar ackVar () >>
        return val

main = getChar >>= \c ->
  do { cVar <- newCVar
     ; forkIO (do { threadDelay 2000000
                  ; val <- takeCVar cVar
                  ; print val}) >>
      putCVar cVar 'f' >>
      print ("produced first value - ", "f") >>
      putCVar cVar c >>
      print ("produced second value - ", c)}
