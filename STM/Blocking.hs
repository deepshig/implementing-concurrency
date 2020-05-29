import Control.Concurrent
import Control.Concurrent.STM

type Account = TVar Int

newAccount :: Int -> IO(Account)
newAccount balance =
   atomically (do { acc <- newTVar balance
                  ; return acc })

getBalance :: Account -> IO Int
getBalance acc =
   atomically (do { bal <- readTVar acc
                  ; return bal})

limitedDeposit :: Account -> Int -> STM ()
limitedDeposit acc amount =
    limitedWithdraw acc (- amount)

limitedWithdraw :: Account -> Int -> STM ()
limitedWithdraw acc amount =
    do { bal <- readTVar acc
       ; check (amount <= bal)
       ; writeTVar acc (bal - amount) }

limitedTransfer :: Account -> Account ->
    Int -> IO ()
limitedTransfer from to amount =
    atomically (do { limitedDeposit to amount
                   ; limitedWithdraw from amount })

-- using block : retry and check
main :: IO()
main =  do  { acc1 <- newAccount 10
            ; acc2 <- newAccount 3
            ; forkIO (do    { print "attempting first transfer"
                            ; limitedTransfer acc2 acc1 5
                            ; print "done first transfer"})
            ; forkIO (do    { threadDelay 2000000
                            ; print "attempting second transfer"
                            ; limitedTransfer acc1 acc2 3
                            ; print "done second transfer"})
            ; threadDelay 4000000
            ; bal1 <- getBalance acc1
            ; bal2 <- getBalance acc2
            ; print ("balance in acc1 : ", bal1)
            ; print ("balance in acc2 : ", bal2)}
