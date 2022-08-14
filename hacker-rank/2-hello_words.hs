{-# LANGUAGE FlexibleInstances, UndecidableInstances, DuplicateRecordFields #-}
{-# LANGUAGE BlockArguments #-}

module Main where

import Control.Monad
import Data.Array
import Data.Bits
import Data.List(take)
--import Data.List.Split
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe

hello_worlds :: Int -> IO ()
hello_worlds 1 = putStrLn "Hello World"
hello_worlds n = do putStrLn "Hello World"; hello_worlds (n-1)

hello_worlds2 :: Int -> IO()
hello_worlds2 n =  mapM_ putStrLn (["Hello World\n"| _<-[1..n]])

print_hello_world :: Int -> String
print_hello_world 1 =  "Hello World"
print_hello_world x = "Hello World\n" ++ print_hello_world (x-1)

--putStrLn (print_hello_world 5)

hello_worlds3 :: Int -> IO ()
hello_worlds3 n = putStrLn $ unlines $ replicate n "Hello World"

hello_worlds4 :: Int -> IO [()]
hello_worlds4 n = sequence . take n . repeat $ putStrLn "Hello World"
--sequence ["AA","BB","CC"]  - > ["ABC","ABC","ABC","ABC","ABC","ABC","ABC","ABC"]

hello_worlds5 :: (Num a, Enum a) => a -> IO ()
hello_worlds5 n = mapM_(\x -> putStrLn "Hello World") [1..n]

hello_worlds6 :: (Eq t, Num t) => t -> IO ()
hello_worlds6 1 = putStrLn "Hello World"
hello_worlds6 n = do 
                hello_worlds6(1)
                hello_worlds6(n - 1) 

hello_worlds7 :: Int -> IO ()
hello_worlds7 n = (putStrLn . unlines . replicate n) "Hello World"

hello_worlds8 n = sequence_ [putStrLn "Hello World" | _ <- [1..n]]

main :: IO()
main = do
    n <- readLn :: IO Int
    hello_worlds 4 
    -- Print "Hello World" on a new line 'n' times.
