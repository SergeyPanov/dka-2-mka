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

-- Construct FSM based on input string
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

-- Merge lists
merge :: [a] -> [a] -> [a]
merge xs     []     = xs
merge []     ys     = ys
merge (x:xs) (y:ys) = x : y : merge xs ys

-- Execute zero undistinguish
zeroIteration :: Automata -> [(State, State)]
zeroIteration fsm = merge fs nfs -- Merged list
    where
        fs = [(p, q) | p <- finits fsm, q <- finits fsm]    -- Finite states
        nfs = [(p, q) | p <- nonFinits fsm, q <- nonFinits fsm] -- Non finite states

-- Create list of transitions at SINK state
createTransitionsToSINK :: [[String]] -> [Transition]
createTransitionsToSINK [] = []
createTransitionsToSINK (x:xs) = (constructTransition x):(createTransitionsToSINK xs)

-- Add SINK state in case of FSM was not complete
addSINK :: Automata -> Automata
addSINK fsm = do
    let
        sinkedRules = unique [ [p, [a],"SINK"] | p <- states fsm, q <- states fsm, a <- alphabet fsm, (length $ sigma fsm p a) == 0 ]
        sinkLoop = unique [["SINK", [a], "SINK"] | a <- alphabet fsm]
        trs = createTransitionsToSINK sinkedRules
        sinkLoopTrs = createTransitionsToSINK sinkLoop

    if length trs > 0
        then Automata (merge (states fsm) ["SINK"]) (alphabet fsm) (start fsm) (merge (merge (transitions fsm)trs) sinkLoopTrs) (finits fsm)
        else do fsm

-- Remove duplicates
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = rdHelper []
    where rdHelper seen [] = seen
          rdHelper seen (x:xs)
              | x `elem` seen = rdHelper seen xs
              | otherwise = rdHelper (seen ++ [x]) xs


-- Remove repeated elements from list
unique :: Ord a => [a] -> [a]
unique l = Set.toList $ Set.fromList l

-- Create set of undistinguished pairs of states
makeUnDistinguishPairs :: [(State, State)] -> Automata -> [(State, State)]
makeUnDistinguishPairs prevPairs fsm
    | (Set.fromList prevPairs) == (Set.fromList nextPairs) = nextPairs
    | otherwise = makeUnDistinguishPairs nextPairs fsm
    where
        newPairs = unique $ [((p, q), a) | (p, q) <- prevPairs, a <- alphabet fsm, ((head (sigma fsm p a) ), (head (sigma fsm q a))) `elem` prevPairs]
        filteredNewPairs = filter ( \((p, q), _) -> (length [((s1, s2), a) | ((s1, s2), a) <- newPairs, s1 == p, s2 == q]) == (length $ alphabet fsm) ) newPairs
        nextPairs = unique $ [(p, q) | ((p, q), a) <- filteredNewPairs]


-- Return class of equivalence for state "s"
getClassForState :: State -> [(State, State)] -> Automata -> EqClass
getClassForState s pairs fsm = EqClass s (unique [q | p <- states fsm, q <- states fsm, p == s, (p, q) `elem` pairs])

-- Based on pairs of undistinguished states gather set of equivalent classes
gatherUndistinguishedCls :: [(State, State)] -> Automata -> [EqClass]
gatherUndistinguishedCls undPairs fsm = ([getClassForState q undPairs fsm| q <- states fsm])



getTransitionsForClass :: EqClass -> [EqClass] -> Automata -> [(EqClass, Symbol, EqClass)]
getTransitionsForClass eqClass allClasses fsm = ([(eqClass, a, dst) |dst <- allClasses, from <- allClasses , eqClass == from, a <- alphabet fsm, Transition (state eqClass) a (state dst) `elem` transitions fsm])

-- Make transitions for new automata
gatherNewTransitions  :: [EqClass] -> Automata -> [[(EqClass, Symbol, EqClass)]]
gatherNewTransitions eqClasses fsm = [getTransitionsForClass cls eqClasses fsm | cls <- eqClasses]

-- Remove duplicate transitions
filterSameTransitions :: Eq a => [a] -> [a]
filterSameTransitions trs = removeDuplicates trs

-- Create transition between equivalence classes based on assignment
-- transitionForEqClasses :: (EqClass, Symbol, EqClass) -> Transition
-- transitionForEqClasses (fromClass, a, toClass) = do
--     let
--         from = Set.fromLi
-- Execute minimization; parameter -t
minimize :: String -> IO()
minimize input = do
    let
        fsm = addSINK $ makeFSM input -- Create DFA
        zeroUndistinguishedPairs = zeroIteration $ addSINK fsm -- Create zero undistinguished pairs of states
        pairs = makeUnDistinguishPairs zeroUndistinguishedPairs fsm -- Create undistinguished pairs of states
        eqClasses = gatherUndistinguishedCls pairs fsm -- Create equivalence classes
        newTransitions = gatherNewTransitions eqClasses fsm -- Create new transitions
        filteredTransitions = [filterSameTransitions trs | trs <- newTransitions]   -- Remove duplicated transitions
        mergedTransitions = removeDuplicates $ foldl (merge) [] filteredTransitions -- Merge lists of transitions to one list and remove duplicated transitions



    print $  mergedTransitions
    print $ "--------------"
    -- print $ filteredTransitions
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