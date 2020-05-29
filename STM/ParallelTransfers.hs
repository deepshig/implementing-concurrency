import Control.Concurrent
import Control.Concurrent.STM

type Account = TVar Int

newAccount :: Int -> IO(Account)
newAccount balance =
   atomically (do { acc <- newTVar balance
                  ; return acc })

withdraw :: Account -> Int -> STM ()
withdraw acc amount =
   do { bal <- readTVar acc
      ; writeTVar acc (bal - amount) }

deposit :: Account -> Int -> STM ()
deposit acc amount = withdraw acc (- amount)

getBalance :: Account -> IO Int
getBalance acc =
   atomically (do { bal <- readTVar acc
                  ; return bal})

transfer :: Account -> Account -> Int -> IO ()
transfer from to amount =
   atomically (do { deposit to amount
                  ; withdraw from amount })

-- runnning 2 parallel transfers
main :: IO()
main = do { acc1 <- newAccount 10
          ; acc2 <- newAccount 3
          ; forkIO (transfer acc1 acc2 5)
          ; forkIO (transfer acc2 acc1 2)
          ; threadDelay 1000000
          ; bal1 <- getBalance acc1
          ; bal2 <- getBalance acc2
          ; print ("balance in acc1 : ", bal1)
          ; print ("balance in acc2 : ", bal2)}
