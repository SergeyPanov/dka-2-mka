module Main where
import System.Environment
import System.IO
import System.IO.Error
import qualified Data.Set as Set

-- Hold input parameters
dispatch :: [([Char], String -> IO ())]
dispatch = [
    ("-i", readAndPrint),
    ("-t", minimize)
    ]

-- Split string on comma ','
split :: String -> [String]
split [] = [""]
split (c:cs)
   | c == ',' = "" : rest
   | otherwise = (c : head rest) : tail rest
   where
       rest = split cs


-- Create tuple with rule
tuplify3 :: [String] -> (String, String, String)
tuplify3 [q, a, p] = (q, a, p)

-- Execute reading and printing
readAndPrint :: String -> IO()
readAndPrint input = do
    let lns = lines input
        (states:start:finits:rules) = lns
        listsOfRules = fmap split rules
        tuples = fmap tuplify3 listsOfRules
    print $ split states
    print $ split start
    print $ split finits
    -- print listsOfRules
    print tuples
    -- print rules


-- Execute minimization
minimize :: String -> IO()
minimize x = do
    putStr $ x ++ " minimize"

-- Read DKA from the file
readFromFile :: String -> IO()
readFromFile file = do
    withFile  file ReadMode (\handle -> do
                                            contents <- hGetContents handle
                                            readAndPrint contents
                                            )
-- Read DKA from stdin
readFromStdIn :: IO()
readFromStdIn = do
        contents <- getContents
        putStrLn contents


main = do
    (command:source) <- getArgs
    putStrLn $ "The command " ++  command

    let (Just action) = lookup command dispatch

    if length source == 0
        then readFromStdIn
        else do readFromFile (head source)
            

    putStrLn "End main"