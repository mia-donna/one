module Main where

import System.Random
import Control.Concurrent  ( threadDelay, forkIO , takeMVar , putMVar , newEmptyMVar , MVar , newMVar , readMVar )
import Control.Monad (forM_ )


-- DATA TYPES
data Customer = Customer {
  name :: Name,
  balance :: Balance,
  account :: Account
} deriving (Eq, Show)
 
type Account = Int
type Balance =  Int
type Name = String
type Value = Int

data Coin = Head | Tail deriving (Show, Eq)  

-- RANDOM GENERATOR FUNCTIONS
coinFlip :: IO Coin
coinFlip = do
    r <- randomIO :: IO Bool
    return $ if r then Head else Tail

randomCustIndex :: IO Int 
randomCustIndex = do
    r <- randomRIO (0, 3)
    return r    

randomAmount :: IO Int 
randomAmount = do
    r <- randomRIO (10, 50)
    return r    

-- TRANSFER FUNCTION
transfer :: Customer -> Customer -> Int -> IO (Customer, Customer)
transfer from to amount
  | amount <= 0 = return (from, to)
  | balance from < amount = return (from, to) -- maybe remove this
  | balance from <= 0  = return (from, to) -- i added this to try to fit r'qs -- a transaction is processed but no money is passed if account balance is 0
  | name from == name to = return (from, to)  -- i added this to try to fit r'qs -- customers to return even if it's the same account -- though we have caught these cases in main
  | otherwise = return ((from { balance =  ((balance  from) - amount)}),(to { balance  =  ((balance  to) + amount)}))


-- THREAD PROCESS
-- customer : the thread that is running  
-- mvar : a separate mvar for the customer thread that is running -> in main these are one, two , three, four etc that we use to build a list in main
-- customerlist : a shared mvar for all customers, that we use to pull them out of one by one, as they get HEAD
-- value : a separate mvar for the random index value 
-- TBC amount : a shared mvar for random transfer amount 
process :: Customer -> MVar Customer -> MVar Value -> MVar Customer -> MVar Customer -> MVar Bool -> IO () 
process customer mvar value customerlist b c = do
    c1 <- coinFlip
    putStrLn $ (show customer) ++ "'s turn, they -- got " ++ (show c1) 
       
    if c1 == Head then do
        putMVar mvar customer
        putMVar customerlist customer
        r2 <- randomCustIndex
        putStrLn $ (show customer) ++ " -- got " ++ (show r2) -- value tests | unblock
        putMVar value r2
    else do    
        randomRIO (1,50) >>= \r -> threadDelay (r * 100000)
        process customer mvar value customerlist b c
    
    
-- MAIN FUNCTION        
main :: IO ()
main = forM_ [1..10] $ \_ -> do
    putStrLn $ ".******------ WELCOME ------******."   
    let c1 = Customer {name = "C1", balance = 0, account = 1}
    let c2 = Customer {name = "C2", balance = 1000, account = 2} 
    let c3 = Customer {name = "C3", balance = 1000, account = 3}
    let c4 = Customer {name = "C4", balance = 1000, account = 4} 
    putStrLn $ ".******------ CUSTOMERS CREATED ------******." 

    -- MVars for customers
    one <- newEmptyMVar
    two <- newEmptyMVar
    three <- newEmptyMVar
    four <- newEmptyMVar
    -- MVars for index values
    value1 <- newEmptyMVar
    value2 <- newEmptyMVar
    value3 <- newEmptyMVar
    value4 <- newEmptyMVar
    
    customerlist <- newEmptyMVar
    b <- newEmptyMVar
    c <- newEmptyMVar
    putStrLn $ ".******------ EMPTY MVARS CREATED ------******."
    randomRIO (1,50) >>= \r -> threadDelay (r * 100000)
    mapM_ forkIO [process c1 one value1 customerlist b c, process c2 two value2 customerlist b c, process c3 three value3 customerlist b c, process c4 four value4 customerlist b c]
    putStrLn $ ".******------ THREADS RUNNING ------******."

    -- haven't used this
    usecustomers <- newMVar [one, two , three, four]

    
    firsthead <- takeMVar customerlist
    let print_name = print . name
    let reveal_first = print_name firsthead
    putStrLn $ "FIRST HEAD: "  
    reveal_first

    secondhead <- takeMVar customerlist
    let reveal_second = print_name secondhead
    putStrLn $ "SECOND HEAD: "  
    reveal_second
    
    thirdhead <- takeMVar customerlist
    let reveal_third = print_name thirdhead
    putStrLn $ "THIRD HEAD: "  
    reveal_third
    
    fourthhead <- takeMVar customerlist
    let reveal_fourth = print_name fourthhead
    putStrLn $ "FOURTH HEAD: "  
    reveal_fourth


   -- INDEX value tests | unblock -- this way we can read each value each customer thread got
    rvalue1 <- readMVar value1
    putStrLn $ show rvalue1

    rvalue2 <- readMVar value2
    putStrLn $ show rvalue2

    rvalue3 <- readMVar value3
    putStrLn $ show rvalue3

    rvalue4 <- readMVar value4
    putStrLn $ show rvalue4

    -- TRANSFER tests | unblock -- a) we take the list of customers we've created from each thread (customerlist) b) we use the first customers random value to index a customer on the list, they become the lucky recipient 
    -- c) then we take the first customer out the box that got head d) they become the payee -- ** this way, all customers get to transfer but not all get transferred each time
    
-- || INDEXING FOR TRANSFERS   
    c <- takeMVar usecustomers -- c :: [MVar Customer]
    
    let index = (c!!rvalue1)
    z <- readMVar index 
    putStrLn $ show z 
    n <- randomAmount

    let index2 = (c!!rvalue2)
    y <- readMVar index2

    let index3 = (c!!rvalue3)
    w <- readMVar index3

    let index4 = (c!!rvalue4)
    v <- readMVar index4



-- || TRANSFERS : with else ifs to catch customers from choosing themselves
-- || FIRST
    if firsthead /= z then do
       (firsthead, z) <- transfer firsthead z n
       putStrLn $ "SUCCESS ****TRANSFER 1****  TRANSFERER DETAILS: " ++ (show firsthead)  ++ " RECIPIENT DETAILS : " ++ (show z)    
     else do 
         putStrLn $ "ERROR ****TRANSFER 1**** woops - two matching customers so no transfer - finding a new customer to transfer to ... "
         if rvalue1 == 3 then do -- number of indexes
            let rvalue1_ = rvalue1 - 1 -- minus one to random index so the next customer gets the transfer
            let index_ = (c!!rvalue1_)
            z_ <- readMVar index_
            (firsthead, z_) <- transfer firsthead z_ n 
            putStrLn $ "SUCCESS ****TRANSFER 1****  TRANSFERER DETAILS: " ++ (show firsthead)  ++ " RECIPIENT DETAILS : " ++ (show z_)
          else do
            let rvalue1_ = rvalue1 + 1 -- add one to random index so the next customer gets the transfer
            let index_ = (c!!rvalue1_)
            z_ <- readMVar index_
            (firsthead, z_) <- transfer firsthead z_ n 
            putStrLn $ "SUCCESS ****TRANSFER 1****  TRANSFERER DETAILS: " ++ (show firsthead)  ++ " RECIPIENT DETAILS : " ++ (show z_)
-- || SECOND 
    if secondhead /= y then do 
       n <- randomAmount
       (secondhead, y) <- transfer secondhead y n
       putStrLn $ "SUCCESS ****TRANSFER 2****  TRANSFERER DETAILS: " ++ (show secondhead) ++ " RECIPIENT DETAILS : " ++ (show y)
     else do
         putStrLn $ "ERROR ****TRANSFER 2**** woops - two matching customers so no transfer - finding a new customer to transfer to ..."
         if rvalue2 == 3 then do 
            let rvalue2_ = rvalue2 - 1 
            let index2_ = (c!!rvalue2_)
            y_ <- readMVar index2_
            (secondhead, y_) <- transfer secondhead y_ n 
            putStrLn $ "SUCCESS ****TRANSFER 2****  TRANSFERER DETAILS: " ++ (show secondhead)  ++ " RECIPIENT DETAILS : " ++ (show y_)
          else do
            let rvalue2_ = rvalue2 + 1 
            let index2_ = (c!!rvalue2_)
            y_ <- readMVar index2_
            (secondhead, y_) <- transfer firsthead y_ n 
            putStrLn $ "SUCCESS ****TRANSFER 1****  TRANSFERER DETAILS: " ++ (show secondhead)  ++ " RECIPIENT DETAILS : " ++ (show y_)
-- || THIRD
    if thirdhead /= w then do 
       n <- randomAmount
       (thirdhead, w) <- transfer thirdhead w n
       putStrLn $ "SUCCESS ****TRANSFER 3****  TRANSFERER DETAILS : " ++ (show thirdhead) ++ " RECIPIENT DETAILS : " ++ (show w) 
     else do 
         putStrLn $ "ERROR ****TRANSFER 3**** woops - two matching customers so no transfer - finding a new customer to transfer to ..."
         if rvalue3 == 3 then do
            let rvalue3_ = rvalue3 - 1 
            let index3_ = (c!!rvalue3_)
            w_ <- readMVar index3_
            (thirdhead, w_) <- transfer thirdhead w_ n 
            putStrLn $ "SUCCESS ****TRANSFER 3****  TRANSFERER DETAILS: " ++ (show thirdhead)  ++ " RECIPIENT DETAILS : " ++ (show w_)
          else do
            let rvalue3_ = rvalue3 + 1 
            let index3_ = (c!!rvalue3_)
            w_ <- readMVar index3_
            (thirdhead, w_) <- transfer thirdhead w_ n 
            putStrLn $ "SUCCESS ****TRANSFER 3****  TRANSFERER DETAILS: " ++ (show thirdhead)  ++ " RECIPIENT DETAILS : " ++ (show w_)
-- || FOURTH

    if fourthhead /= v then do 
       n <- randomAmount
       (fourthhead, v) <- transfer fourthhead v n 
       putStrLn $ "SUCCESS ****TRANSFER 4****  TRANSFERER DETAILS : " ++ (show fourthhead) ++ " RECIPIENT DETAILS : " ++ (show v)
     else do 
         putStrLn $ "ERROR ****TRANSFER 4**** woops - two matching customers so no transfer - finding a new customer to transfer to ..."
         if rvalue4 == 3 then do
            let rvalue4_ = rvalue4 - 1 
            let index4_ = (c!!rvalue4_)
            v_ <- readMVar index4_
            (secondhead, v_) <- transfer fourthhead v_ n 
            putStrLn $ "SUCCESS ****TRANSFER 4****  TRANSFERER DETAILS: " ++ (show fourthhead)  ++ " RECIPIENT DETAILS : " ++ (show v_)
          else do
            let rvalue4_ = rvalue4 + 1 
            let index4_ = (c!!rvalue4_)
            v_ <- readMVar index4_
            (fourthhead, v_) <- transfer fourthhead v_ n 
            putStrLn $ "SUCCESS ****TRANSFER 4****  TRANSFERER DETAILS: " ++ (show fourthhead)  ++ " RECIPIENT DETAILS : " ++ (show v_)





    {-
    putStrLn $  "****FINAL BALANCES**** " ++  (show firsthead)
    putStrLn $  "****FINAL BALANCES**** " ++  (show secondhead)
    putStrLn $  "****FINAL BALANCES**** " ++  (show thirdhead)
    putStrLn $  "****FINAL BALANCES**** " ++  (show fourthhead)-}

    putStrLn $ ".******------ TEST || THREADS ALL RUN - EXIT ------******."

    








    -- below : something i was trying on friday
{-process :: Name -> Customers -> Customer -> MVar Customer -> MVar (Customers, Customers) -> MVar Value -> IO () 
process name customers cust customerbox customersbox value = do
    r1 <- diceThrow
    putMVar customerbox cust
    putStrLn $ name ++ "'s turn"
    putStrLn $ (show cust) ++ " -- got " ++ (show r1)
    if r1 /= customers then 
        putMVar customersbox (customers , r1 )
    else do 
        putStrLn $ " -- playing again "
        threadDelay 100
        process name customers cust customerbox customersbox value-}
    
    
    