{-# LANGUAGE InstanceSigs #-}
module Automata  where

import qualified Data.Set as Set


-- Create aliases for convinience
type State = String -- Alias for state
type Symbol = Char -- Alias for input symbol from alphabet


data Transition = Transition{from :: State
                            , input :: Symbol
                            , to :: State
                            } deriving (Show) -- Custom data type for transition

data Automata = Automata {states :: [State]
                        , alphabet :: [Symbol]
                        , start :: State
                        , transitions :: [Transition]
                        , finits :: [State]
                        } deriving(Show) -- Custom datatype for fsm

-- Return list of "to" states
sigma :: Automata -> State -> Symbol -> [State]
sigma fsm f i = [q | Transition p a q <- transitions fsm, p == f, i == a]

-- Return list of non finit states
nonFinits :: Automata -> [State]
nonFinits fsm = Set.toList ( (Set.fromList $ states fsm) `Set.difference` (Set.fromList $ finits fsm) )

instance Eq Transition where
    (==) :: Transition -> Transition -> Bool
    t1 == t2 = from t1 == from t2 && input t1 == input t2 && to t1 == to t2