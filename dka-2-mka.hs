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


-- Return second element from tuple
getFirst :: (String, String, String) -> String
getFirst (x, _, _) = x

-- Return second element from tuple
getSecond :: (String, String, String) -> String
getSecond (_, x, _) = x

-- Return third element from tuple
getThird :: (String, String, String) -> Maybe String
getThird (_, _, x) = Just x

-- Create tuple with rule
tuplify3 :: [String] -> (String, String, String)
tuplify3 [q, a, p] = (q, a, p)

-- Return whole alphabet based on rules
getAlphabet :: [(String, String, String)] -> [String]
getAlphabet [] = []
getAlphabet (x:xs) = (getSecond x):(getAlphabet xs)

-- Create the DKA structure based on definition
makeDKA :: String -> ([String], [String], String, [String], [(String, String, String)])
makeDKA input =
    let lns = lines input
        (states:start:finits:rules) = lns
        listsOfRules = fmap split rules
        tuples = fmap tuplify3 listsOfRules
    in ( Set.toList $ (Set.fromList (split states)), Set.toList (Set.fromList (getAlphabet tuples)), start, Set.toList (Set.fromList (split finits)), Set.toList (Set.fromList tuples) )

-- Execute reading and printing; parameter -i
readAndPrint :: String -> IO()
readAndPrint input = do print $ makeDKA input
    -- print $ split states
    -- print $ split start
    -- print $ split finits
    -- print listsOfRules
    -- print tuples
    -- print rules

-- -- Execute reading and printing
-- readAndPrint :: String -> IO()
-- readAndPrint input = do
--     let lns = lines input
--         (states:start:finits:rules) = lns
--         listsOfRules = fmap split rules
--         tuples = fmap tuplify3 listsOfRules
--     print $ split states
--     print $ split start
--     print $ split finits
--     -- print listsOfRules
--     print tuples
--     -- print rules


-- distinguish :: 
-- distinguish 

-- Sigma function(rules)
sigma :: [(String, String, String)] -> (String, String) -> Maybe String
sigma rules (state, symbol)  = 
    if ( length [p | (q, a, p) <- rules, q == state, a == symbol] ) > 0
        then Just (head $ [p | (q, a, p) <- rules, q == state, a == symbol])    -- Return Just target state
        else Nothing -- Return nothing

-- Execute minimization; parameter -t
-- TODO: distinguish function
minimize :: String -> IO()
minimize input = do
    let
        (states, alphabet, start, finits, rules) = makeDKA input -- Create DKA based on definition
        pairs = [(x, y) | x <- states, y <- alphabet] -- Create pairs of rules
        notFinit = Set.toList ( (Set.fromList states) `Set.difference` (Set.fromList finits) )
        zeroEq = [finits, notFinit]
        targets =  (map (sigma rules ) pairs ) -- Target states


    print $ zeroEq
    print "-----------"
    print $ notFinit
    print "-----------"
    print $ rules
    print "-----------"
    print $ pairs
    print "-----------"
    print $ targets
    return ()
    


-- Read DKA from the file
readFromFile :: ( String -> IO() ) -> FilePath -> IO()
readFromFile action file = do
    withFile  file ReadMode (\handle -> do
                                            contents <- hGetContents handle
                                            action contents
                                            )

-- Read DKA from stdin
readFromStdIn :: ( String -> IO() ) -> IO()
readFromStdIn action = do
        contents <- getContents
        action contents


main = do
    (command:source) <- getArgs
    putStrLn $ "The command " ++  command

    let (Just action) = lookup command dispatch

    if length source == 0   -- If source file was not set
        then readFromStdIn action   -- Reading from stdin
        else do readFromFile action (head source)   -- Reading from file
            

    putStrLn "End main"