import Control.Concurrent

-- it is like double-linked list :
-- readVar -> start pointer,
-- writeVar -> end pointer,
-- stream -> list of items,
-- item -> (head, rest of the list)
type Channel a = (MVar (Stream a),
                  MVar (Stream a))
type Stream a = MVar (Item a)
data Item a = Item a (Stream a)

newChannel :: IO (Channel a)
newChannel = newEmptyMVar >>= \read ->
   newEmptyMVar >>= \write ->
      newEmptyMVar >>= \emptyStream ->
         putMVar read emptyStream >>
         putMVar write emptyStream >>
         return (read,write)

putChannel :: Channel a -> a -> IO()
putChannel (read,write) val =
   newEmptyMVar >>= \newItem ->
      takeMVar write >>= \oldStream ->
         putMVar write newItem >>
         putMVar oldStream (Item val newItem)

getChannel :: Channel a -> IO a
getChannel (read,write) =
   takeMVar read >>= \head ->
      takeMVar head >>= \(Item val restStream) ->
         putMVar read restStream >>
         return val

main = getChar >>= \c ->
 do { channel <- newChannel
    ; forkIO (
       do {
        threadDelay 2000000
        ; val <- getChannel channel
        ; print ("value read in fork", val)}) >>
      putChannel channel 'f' >>
      print "put first value - 'f'" >>
      putChannel channel c >>
      print ("put second value - ", c) >>
      do {
       threadDelay 3000000
       ; val <- getChannel channel
       ; print ("value left in channel", val)}}

