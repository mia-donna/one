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

data Customers = C1 | C2 | C3 | C4 deriving (Show, Eq)

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
    
process :: Customer -> MVar Customer -> MVar Value -> MVar Customer -> MVar Customer -> MVar Bool -> IO () 
process customer mvar value a b c = do
    r1 <- diceThrow 
    putMVar mvar customer
    putStrLn $ (show customer) ++ " -- got " ++ (show r1) 
    
    {-if r1 == C1 then do
        putStrLn $ (show customer) ++ " -- got C1 test"
        
     else do 
       putStrLn $ (show customer) ++ " -- got another customer"   -}
    
        

main :: IO ()
main = do
    putStrLn $ ".******------ WELCOME ------******."   
    let c1 = Customer {name = "C1", balance = 100, account = 1}
    let c2 = Customer {name = "C2", balance = 100, account = 2} 
    let c3 = Customer {name = "C3", balance = 100, account = 3}
    let c4 = Customer {name = "C4", balance = 20, account = 4} 
    putStrLn $ ".******------ CUSTOMERS CREATED ------******." 
    one <- newEmptyMVar
    two <- newEmptyMVar
    three <- newEmptyMVar
    four <- newEmptyMVar
    value <- newEmptyMVar
    a <- newEmptyMVar
    b <- newEmptyMVar
    c <- newEmptyMVar
    putStrLn $ ".******------ EMPTY MVARS CREATED ------******."
    mapM_ forkIO [process c1 one value a b c, process c2 two value a b c, process c3 three value a b c, process c4 four value a b c]
    putStrLn $ ".******------ THREADS RUN ------******."

    --putMVar value 5
    v <- takeMVar value -- having this at the end means the main thread is blocked i.e. all threads run, it's waiting for something
    putStrLn $ ".******------ TEST - EXIT ------******."

    













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
    
    
    