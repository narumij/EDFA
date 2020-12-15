module DFA (
  State(..),
  Move(..),
  Graph'(..),
  Graph(..),
  Edges(..),
  state,
  stateContents,
  edges,
  ) where

data Move input state
  = Input [(input,state)]
  | Eplision state
  deriving (Eq,Show)

newtype State stateContents
  = State stateContents
  deriving (Eq, Show)

data Graph' state edges
  = Node state edges
  | Leaf state
  deriving (Eq, Show)

type Edges state input = Move input (State state)
type Graph state input = Graph' (State state) (Edges state input)

state :: Graph' state edges -> state
state (Node s _) = s
state (Leaf s) = s

edges :: Graph' state edges -> Maybe edges
edges (Node _ e) = Just e
edges (Leaf _) = Nothing

-- stateContents :: Graph' (State stateContents) e -> stateContents
-- stateContents (Node (State s) _) = s
-- stateContents (Leaf (State s)  ) = s

stateContents :: State stateContents -> stateContents
stateContents (State s) = s
