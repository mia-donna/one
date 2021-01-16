module Main where

import System.Random
import Control.Concurrent  ( threadDelay, forkIO , takeMVar , putMVar , newEmptyMVar , MVar , newMVar , readMVar )
import Control.Monad (replicateM)


-- TYPES
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
data Customers = C1 | C2 | C3 | C4 deriving (Show, Eq)

coinFlip :: IO Coin
coinFlip = do
    r <- randomIO :: IO Bool
    return $ if r then Head else Tail

mapIntToCustomers :: Int -> Customers
mapIntToCustomers n = case r of
      0 -> C1
      1 -> C2
      2 -> C3
      3 -> C4
    where r = mod n 4

diceThrow :: IO Customers
diceThrow = do
    n <- randomIO :: IO Int
    let dice = mapIntToCustomers n
    return dice

randomCustIndex :: IO Int 
randomCustIndex = do
    r <- randomRIO (0, 3)
    return r    
    
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
        randomRIO (1,10) >>= \r -> threadDelay (r * 100000)
        process customer mvar value customerlist b c
    
-- TRANSFER FUNCTION
transfer :: Customer -> Customer -> Int -> IO (Customer, Customer)
transfer from to amount
  | amount <= 0 = return (from, to)
  | balance from < amount = return (from, to) -- maybe remove this
  -- | name from == name to = return (from, to)  -- i added this to try to get customers to return even if it's the same
  | otherwise = return ((from { balance =  ((balance  from) - amount)}),(to { balance  =  ((balance  to) + amount)}))   
    
    

   
    
        

main :: IO ()
main = do
    putStrLn $ ".******------ WELCOME ------******."   
    let c1 = Customer {name = "C1", balance = 100, account = 1}
    let c2 = Customer {name = "C2", balance = 100, account = 2} 
    let c3 = Customer {name = "C3", balance = 100, account = 3}
    let c4 = Customer {name = "C4", balance = 100, account = 4} 
    putStrLn $ ".******------ CUSTOMERS CREATED ------******." 
    one <- newEmptyMVar
    two <- newEmptyMVar
    three <- newEmptyMVar
    four <- newEmptyMVar

    value1 <- newEmptyMVar
    value2 <- newEmptyMVar
    value3 <- newEmptyMVar
    value4 <- newEmptyMVar
    
    customerlist <- newEmptyMVar
    b <- newEmptyMVar
    c <- newEmptyMVar
    putStrLn $ ".******------ EMPTY MVARS CREATED ------******."
    mapM_ forkIO [process c1 one value1 customerlist b c, process c2 two value2 customerlist b c, process c3 three value3 customerlist b c, process c4 four value4 customerlist b c]
    putStrLn $ ".******------ THREADS RUNNING ------******."


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

    -- TRANSFER tests | unblock -- a) we take the list of customers we've created from each thread (usecustomers) b) we use the first customers random value to index a customer on the list, they become the lucky recipient 
    -- c) then we take the first customer out the box that got head d) they become the payee -- ** this way, all customers get transfers and transferred 
    
    c <- takeMVar usecustomers -- c :: [MVar Customer]
    let index = (c!!rvalue1)
    z <- readMVar index -- changing take to read
    putStrLn $ show z 
    (firsthead, z) <- transfer firsthead z 29
    --putStrLn $ "****TRANSFER 1****  RECIPIENT DETAILS: " ++ (show z) ++ " TRANSFERER DETAILS : " ++ (show firsthead)
    putStrLn $ "****TRANSFER 1****  TRANSFERER DETAILS: " ++ (show firsthead)  ++ " RECIPIENT DETAILS : " ++ (show z)    

    
    let index2 = (c!!rvalue2)
    y <- readMVar index2
    putStrLn $ show y 
    (secondhead, y) <- transfer secondhead y 29
    --putStrLn $ "****TRANSFER 2****  RECIPIENT DETAILS: " ++ (show y) ++ " TRANSFERER DETAILS : " ++ (show secondhead)
    putStrLn $ "****TRANSFER 2****  TRANSFERER DETAILS: " ++ (show secondhead) ++ " RECIPIENT DETAILS : " ++ (show y)

    let index3 = (c!!rvalue3)
    w <- readMVar index3
    putStrLn $ show w 
    (thirdhead, w) <- transfer thirdhead w 29
    --putStrLn $ "****TRANSFER 3****  RECIPIENT DETAILS: " ++ (show w) ++ " TRANSFERER DETAILS : " ++ (show thirdhead)
    putStrLn $ "****TRANSFER 3****  TRANSFERER DETAILS : " ++ (show thirdhead) ++ " RECIPIENT DETAILS : " ++ (show w) 

    let index4 = (c!!rvalue4)
    v <- readMVar index4
    putStrLn $ show v 
    (fourthhead, v) <- transfer fourthhead v 29
    --putStrLn $ "****TRANSFER 4****  RECIPIENT DETAILS: " ++ (show v) ++ " TRANSFERER DETAILS : " ++ (show fourthhead)
    putStrLn $ "****TRANSFER 4****  TRANSFERER DETAILS : " ++ (show fourthhead) ++ " RECIPIENT DETAILS : " ++ (show v)
    -- c <- takeMVar customerlist -- having this at the end means the main thread is blocked i.e. all threads run, it's waiting for something | having it full means program can finish || doesn't work if run all 4 head's

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
    
    
    