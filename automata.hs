module Automata  where


-- Create aliases for convinience
type State = String -- Alias for state
type Symbol = Char -- Alias for input symbol from alphabet


data Transition = Transition{from :: State
                            , input :: Symbol
                            , to :: State
                            } deriving (Show) -- Alias for transition

data Automata = Automata {states :: [State]
                        , alphabeth :: [Symbol]
                        , start :: State
                        , transitions :: [Transition]
                        , finits :: [State]
                        } deriving(Show)