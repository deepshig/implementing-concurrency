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

limitedWithdraw :: Account -> Int -> STM ()
limitedWithdraw acc amount =
    do { bal <- readTVar acc
       ; check (amount <= bal)
       ; writeTVar acc (bal - amount) }

choiceWithdraw :: Account -> Account ->
                  Int -> STM ()
-- (choiceWithdraw acc1 acc2 amt)
-- withdraws amt from acc1,
-- if acc1 has enough money, else from acc2.
-- If neither has enough, it retries.
choiceWithdraw acc1 acc2 amt =
    orElse (limitedWithdraw acc1 amt)
           (limitedWithdraw acc2 amt)

choiceDeposit :: Account -> Int -> STM ()
choiceDeposit acc amount =
    choiceWithdraw acc acc (- amount)

choiceTransfer :: Account -> Account ->
                  Account -> Int -> IO ()
choiceTransfer from1 from2 to amount =
    atomically (
       do { choiceDeposit to amount
          ; choiceWithdraw from1 from2 amount})

-- implementing choice : orElse
main :: IO()
main =  do  { acc1 <- newAccount 10
            ; acc2 <- newAccount 3
            ; acc3 <- newAccount 7
            ; choiceTransfer acc2 acc3 acc1 5
            ; bal1 <- getBalance acc1
            ; bal2 <- getBalance acc2
            ; bal3 <- getBalance acc3
            ; print ("balance in acc1 : ", bal1)
            ; print ("balance in acc2 : ", bal2)
            ; print ("balance in acc3 : ", bal3)}
