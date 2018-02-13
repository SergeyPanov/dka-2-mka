module Main where
import System.Environment
import System.IO
import System.IO.Error
import qualified Data.Set as Set
import qualified Data.List as List
import Automata

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
getFrom :: Transition -> State
getFrom tr = from tr

-- Return second element from tuple
getInput :: Transition -> Symbol
getInput tr = input tr

-- Return third element from tuple
getTo :: Transition -> State
getTo tr = to tr

-- Create tuple with rule
constructTransition :: [String] -> Transition
constructTransition [q, a, p] = Transition q (head $ a) p

-- Return whole alphabet based on rules
getAlphabet :: [Transition] -> [Symbol]
getAlphabet [] = []
getAlphabet (x:xs) = (getInput x):(getAlphabet xs)

-- Create string from transition
stringifyTransition :: Transition -> String
stringifyTransition tr = (from tr) ++ "," ++ (\c -> [c]) (input tr) ++ "," ++ (to tr)

-- makeDKA :: String -> Automata
makeFSM input =
    let lns = lines input
        (states:start:finits:rules) = lns
        listsOfRules = fmap split ( Set.toList $ Set.fromList rules)
        tuples = fmap constructTransition listsOfRules
    in Automata (Set.toList (Set.fromList (split states))) (Set.toList (Set.fromList (getAlphabet tuples))) (start) (tuples) (Set.toList (Set.fromList (split finits)))
    -- in ( Set.toList $ (Set.fromList (split states)), Set.toList (Set.fromList (getAlphabet tuples)), start, Set.toList (Set.fromList (split finits)), Set.toList (Set.fromList tuples) )


-- Create the DKA structure based on definition
-- makeDKA :: String -> ([String], [String], String, [String], [(String, String, String)])
-- makeDKA input =
--     let lns = lines input
--         (states:start:finits:rules) = lns
--         listsOfRules = fmap split rules
--         tuples = fmap constructTransition listsOfRules
--     in ( Set.toList $ (Set.fromList (split states)), Set.toList (Set.fromList (getAlphabet tuples)), start, Set.toList (Set.fromList (split finits)), Set.toList (Set.fromList tuples) )

-- Execute reading and printing; parameter -i
readAndPrint :: String -> IO()
readAndPrint input = do
    let
        fsm = makeFSM input
        sts = states fsm
        strt = start fsm
        fnts = finits fsm
        trns = transitions fsm
        stringifyedTransitions = map stringifyTransition trns

    putStrLn $ id $ List.intercalate "," sts
    putStrLn $ id strt
    putStrLn $ id $ List.intercalate "," fnts
    putStrLn $ id $ List.intercalate "\n" stringifyedTransitions
    return()

-- -- undistinguished ::
-- undistinguished (states, alphabet, start, finits, rules) oldPairs newPairs
--     | oldPairs == newPairs = newPairs
--     | otherwise = undistinguished (states, alphabet, start, finits, rules) newPairs [(p, q) | (p, q) <- newPairs, a <- alphabet,  ( ( (sigma rules (p, a)), (sigma rules (q, a)) ) `elem` newPairs ) ]

-- Table filling algorithm
-- tableFilling :: 
tableFilling (states, alphabet, start, finits, rules) = do
    let
        nonFinits = Set.toList ( (Set.fromList states) `Set.difference` (Set.fromList finits) )
        distinguishableF = [(p, q) | p <- finits, q <- finits]
        distinguishableNF = [(p, q) | p <- nonFinits, q <- nonFinits]
        distinguishable = Set.toList (Set.fromList distinguishableF `Set.union` Set.fromList distinguishableNF)
        -- aux = undistinguished (states, alphabet, start, finits, rules) [] distinguishable

    print $ distinguishable
    print "-----------"
    print $ finits
    print "-----------"
    print $ nonFinits

    return()

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
    return()
    -- let
        -- (states, alphabet, start, finits, rules) = makeDKA input -- Create DKA based on definition
        -- pairs = [(x, y) | x <- states, y <- alphabet] -- Create pairs of rules
        -- nonFinits = Set.toList ( (Set.fromList states) `Set.difference` (Set.fromList finits) )
        -- zeroEq = [finits, nonFinits]
        -- targets =  (map (sigma rules ) pairs ) -- Target states
        -- distinguishable = [(p, q) | p <- finits, q <- nonFinits]

    -- tableFilling $ (states, alphabet, start, finits, rules)
    -- print $ distinguishable
    -- print "-----------"
    -- print $ finits
    -- print "-----------"
    -- print $ nonFinits
    -- print "-----------"
    -- print $ rules
    -- print "-----------"
    -- print $ pairs
    -- print "-----------"
    -- print $ targets
    -- return ()
    


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