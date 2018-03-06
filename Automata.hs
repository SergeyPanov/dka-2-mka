-- Project: DKA-2-MKA
-- Author: Sergey Panov xpanov00
{-# LANGUAGE InstanceSigs #-}
module Automata  where
import qualified Data.Set as Set


-- Create aliases for convinience
type State = String -- Alias for state
type Symbol = Char -- Alias for input symbol from alphabet

-- Castom type for transition
data Transition = Transition{from :: State
                            , input :: Symbol
                            , to :: State
                            } deriving (Show)
-- Castom type for FA
data Automata = Automata {states :: [State]
                        , alphabet :: [Symbol]
                        , start :: State
                        , transitions :: [Transition]
                        , finits :: [State]
                        } deriving(Show)

-- Castom type for equivalence class
data EqClass = EqClass { state :: State
                        , eqCls :: [State]
                    } deriving (Show, Ord)

-- Return list of "to" states. In case of DFA list will have only one state
sigma :: Automata -> State -> Symbol -> [State]
sigma fsm f i = [q | Transition p a q <- transitions fsm, p == f, i == a]

-- Return list of non finit states
nonFinits :: Automata -> [State]
nonFinits fsm = Set.toList ( (Set.fromList $ states fsm) `Set.difference` (Set.fromList $ finits fsm) )

-- Cleasses are equivalent if sets of states are the same
instance Eq EqClass where
    (==) :: EqClass -> EqClass -> Bool
    c1 == c2 = (Set.fromList $ eqCls c1) == (Set.fromList $ eqCls c2)

-- Transitions (p1, a1, q1) == (p2, a2, q2) are identical iff p1 == p2 && a1 == a2 && q1 == q2
instance Eq Transition where
    (==) :: Transition -> Transition -> Bool
    t1 == t2 = from t1 == from t2 && input t1 == input t2 && to t1 == to t2
