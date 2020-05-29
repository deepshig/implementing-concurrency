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

-- normal implementation
main :: IO()
main =  do  { from <- newAccount 10
            ; to <- newAccount 3
            ; transfer from to 5
            ; fromBalance <- getBalance from
            ; toBalance <- getBalance to
            ; print ("balance in from : ", fromBalance)
            ; print ("balance in to : ", toBalance)}
