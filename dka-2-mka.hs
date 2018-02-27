-- Project: DKA-2-MKA
-- Author: Sergey Panov xpanov00
module Main where
import System.Environment
import System.IO
import System.IO.Error
import qualified Data.Set as Set
import qualified Data.List as List
import Data.Maybe
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
        listsOfRules = fmap split ( removeDuplicates rules)
        tuples = fmap constructTransition listsOfRules
    in Automata (removeDuplicates (split states)) (removeDuplicates (getAlphabet tuples)) (start) (tuples) (removeDuplicates (filter (\st -> (length st) >= 1 )  (split finits)))

-- Execute reading and printing; parameter -i
readAndPrint :: String -> IO()
readAndPrint input = do
    let
        fsm = validateFSM $ makeFSM input
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

-- Execute zero undistinguished
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
        sinkedRules = removeDuplicates [ [p, [a],"SINK"] | p <- states fsm, q <- states fsm, a <- alphabet fsm, (length $ sigma fsm p a) == 0 ]
        sinkLoop = removeDuplicates [["SINK", [a], "SINK"] | a <- alphabet fsm]
        trs = createTransitionsToSINK sinkedRules
        sinkLoopTrs = createTransitionsToSINK sinkLoop

    if length trs > 0
        then Automata (merge (states fsm) ["SINK"]) (alphabet fsm) (start fsm) (merge (merge (transitions fsm)trs) sinkLoopTrs) (finits fsm)
        else do fsm

-- Remove duplicates from list
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = rdHelper []
    where rdHelper seen [] = seen
          rdHelper seen (x:xs)
              | x `elem` seen = rdHelper seen xs
              | otherwise = rdHelper (seen ++ [x]) xs

-- Create set of undistinguished pairs of states
makeUnDistinguishPairs :: [(State, State)] -> Automata -> [(State, State)]
makeUnDistinguishPairs prevPairs fsm
    | (Set.fromList prevPairs) == (Set.fromList nextPairs) = nextPairs
    | otherwise = makeUnDistinguishPairs nextPairs fsm
    where
        newPairs = removeDuplicates $ [((p, q), a) | (p, q) <- prevPairs, a <- alphabet fsm, ((head (sigma fsm p a) ), (head (sigma fsm q a))) `elem` prevPairs]
        filteredNewPairs = filter ( \((p, q), _) -> (length [((s1, s2), a) | ((s1, s2), a) <- newPairs, s1 == p, s2 == q]) == (length $ alphabet fsm) ) newPairs
        nextPairs = removeDuplicates $ [(p, q) | ((p, q), a) <- filteredNewPairs]


-- Return class of equivalence for state "s"
getClassForState :: State -> [(State, State)] -> Automata -> EqClass
getClassForState s pairs fsm = EqClass s (removeDuplicates $ s:[q | p <- states fsm, q <- states fsm, p == s, (p, q) `elem` pairs])

-- Based on pairs of undistinguished states gather set of equivalent classes
gatherUndistinguishedCls :: [(State, State)] -> Automata -> [EqClass]
gatherUndistinguishedCls undPairs fsm = ([getClassForState q undPairs fsm| q <- states fsm])


-- Return transitions for given equivalence class
getTransitionsForClass :: EqClass -> [EqClass] -> Automata -> [(EqClass, Symbol, EqClass)]
getTransitionsForClass eqClass allClasses fsm = ([(eqClass, a, dst) |dst <- allClasses, from <- allClasses , eqClass == from, a <- alphabet fsm, Transition (state eqClass) a (state dst) `elem` transitions fsm])

-- Make transitions for new automata
gatherNewTransitions  :: [EqClass] -> Automata -> [[(EqClass, Symbol, EqClass)]]
gatherNewTransitions eqClasses fsm = [getTransitionsForClass cls eqClasses fsm | cls <- eqClasses]

-- Remove duplicate transitions
filterSameTransitions :: Eq a => [a] -> [a]
filterSameTransitions trs = removeDuplicates trs


-- Extract value from Maybe type or throw exception
extractFromMaybe :: String -> Maybe EqClass -> EqClass
extractFromMaybe def optional = 
    case optional of
        Just value -> value
        Nothing    -> error def

-- Execute validation process
validateFSM :: Automata -> Automata
validateFSM fsm = do
    let
        isValidFinite = (Set.fromList $ finits fsm) `Set.isSubsetOf` (Set.fromList $ states fsm)
        isValidStart = (start fsm) `elem` (states fsm)
        sts = ( map (\tr -> (from tr):(to tr):[]) (transitions fsm) )
        mergedSts = removeDuplicates $ foldl (merge) [] sts
        isValidTransition = (Set.fromList mergedSts) `Set.isSubsetOf` (Set.fromList $ states fsm)

    if isValidStart && isValidFinite && isValidTransition
        then fsm
        else error "Invalid format of input DFA"



-- Execute minimization; parameter -t
minimize :: String -> IO()
minimize input = do
    let
        fsm = addSINK $ validateFSM $ makeFSM input -- Create DFA
        
        zeroUndistinguishedPairs = zeroIteration $ addSINK fsm -- Create zero undistinguished pairs of states
        
        pairs = makeUnDistinguishPairs zeroUndistinguishedPairs fsm -- Create undistinguished pairs of states
        
        eqClasses = gatherUndistinguishedCls pairs fsm -- Create equivalence classes
        
        newTransitions = gatherNewTransitions eqClasses fsm -- Create new transitions
        
        filteredTransitions = [filterSameTransitions trs | trs <- newTransitions]   -- Remove duplicated transitions
        
        mergedTransitions = removeDuplicates $ foldl (merge) [] filteredTransitions -- Merge lists of transitions to one list and remove duplicated transitions
        
        patternedTransitions = map (\(fr, a, to) -> (minimum (eqCls fr), a, minimum (eqCls to))) mergedTransitions -- Transitions gathered to tuples (from, symbol, to)
        
        filteredPatternedTransitions = filter (\(stFrom, symb, stTo) -> stFrom /= "SINK" && stTo /= "SINK") patternedTransitions -- Transitions without "SINK" state

        newStates = map (\cls -> state cls) ( removeDuplicates (filter (\cls -> state cls /= "SINK") eqClasses) )

        finitEqClasses = removeDuplicates $ filter (\eqClass -> (state eqClass) `elem` (finits fsm) ) eqClasses -- Get finite classes of equivalence

        newFinitStates = removeDuplicates $ map (\eqCl -> minimum $ eqCls eqCl ) finitEqClasses  -- Get new finite states

        maybeNewStart =  (List.find (\eqCl -> (state eqCl) == (start fsm)) eqClasses) -- Get new start state

        newStart = minimum $ eqCls ( extractFromMaybe "No new start state" maybeNewStart )-- Get new start state, or ERROR message


    putStrLn $ id $ List.intercalate "," $ List.sort newStates
    putStrLn $ id newStart
    putStrLn $ id $ List.intercalate "," $ List.sort newFinitStates
    putStrLn $ id $ List.intercalate "\n" $ List.sort $ (map stringifyTransition ( map (\(fr, s, to) -> Transition fr s to) filteredPatternedTransitions ))
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

main :: IO()
main = do
    (command:source) <- getArgs
    let (Just action) = lookup command dispatch
    if length source == 0   -- If source file was not set
        then readFromStdIn action   -- Reading from stdin
        else do readFromFile action (head source)   -- Reading from file
