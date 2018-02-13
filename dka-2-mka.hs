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

-- Construct FSM
makeFSM :: String -> Automata
makeFSM input =
    let lns = lines input
        (states:start:finits:rules) = lns
        listsOfRules = fmap split ( Set.toList $ Set.fromList rules)
        tuples = fmap constructTransition listsOfRules
    in Automata (Set.toList (Set.fromList (split states))) (Set.toList (Set.fromList (getAlphabet tuples))) (start) (tuples) (Set.toList (Set.fromList (split finits)))

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

-- Table filling algorithm
-- tableFilling :: 
tableFilling (states, alphabet, start, finits, rules) = do
    let
        nonFinits = Set.toList ( (Set.fromList states) `Set.difference` (Set.fromList finits) )
        distinguishableF = [(p, q) | p <- finits, q <- finits]
        distinguishableNF = [(p, q) | p <- nonFinits, q <- nonFinits]
        distinguishable = Set.toList (Set.fromList distinguishableF `Set.union` Set.fromList distinguishableNF)

    print $ distinguishable
    print "-----------"
    print $ finits
    print "-----------"
    print $ nonFinits

    return()

-- Merge 2 lists
merge :: [a] -> [a] -> [a]
merge xs     []     = xs
merge []     ys     = ys
merge (x:xs) (y:ys) = x : y : merge xs ys

-- Create list of transitions at SINK state
createTransitionsToSINK :: [[String]] -> [Transition]
createTransitionsToSINK [] = []
createTransitionsToSINK (x:xs) = (constructTransition x):(createTransitionsToSINK xs)

-- Add SINK state in case of FSM was not complete
addSINK :: Automata -> Automata
addSINK fsm = do
    let
        sinkedRules = Set.toList $ Set.fromList [ [p, [a],"SINK"] | p <- states fsm, q <- states fsm, a <- alphabet fsm, (length $ sigma fsm p a) == 0 ]
        trs = createTransitionsToSINK sinkedRules

    if length trs > 0
        then Automata (merge (states fsm) ["SINK"]) (alphabet fsm) (start fsm) (merge (transitions fsm)trs) (finits fsm)
        else do fsm

-- Execute minimization; parameter -t
minimize :: String -> IO()
minimize input = do
    print $ addSINK $ makeFSM input
    putStrLn "minimize"
    return()



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