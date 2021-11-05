module Hlist where

main :: IO ()
main = do
    putStrLn $ foldr (+) 0 [0..10]
    putStrLn "Hello"
