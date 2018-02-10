module Main where
import System.Environment
import System.IO
import System.IO.Error
import qualified Data.Set as Set  

dispatch :: [([Char], String -> IO ())]
dispatch = [
    ("-i", readAndPrint),
    ("-t", minimize)
    ]

-- Execute reading and printing
readAndPrint :: String -> IO()
readAndPrint x = do
    putStr $ x ++ " readAndPrint"


-- Execute minimization
minimize :: String -> IO()
minimize x = do
    putStr $ x ++ " minimize"


readFromFile :: String -> IO()
readFromFile x = putStrLn $ x ++  " read from file"

-- readFromStdIn :: IO()
readFromStdIn = do
        contents <- getContents
        putStrLn contents

-- readDKA :: IO() -> [t]
-- readDKA handle = do

main = do
    (command:source) <- getArgs
    putStrLn command

    let (Just action) = lookup command dispatch

    if length source == 0
        then readFromStdIn
        else do
            withFile  (head source) ReadMode (\handle -> do
                                            contents <- hGetContents handle
                                            putStrLn contents
                                            )

    putStrLn "Hello"