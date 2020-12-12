module DFA (
  State(..),
  Moves(..),
  Graph'(..),
  Graph(..),
  Edge(..),
  ) where

data Moves input state
  = Input [(input,state)]
  | Eplision state
  deriving (Eq,Show)

newtype State state
  = State state
  deriving (Eq, Show)

data Graph' state edges
  = Node {
      state :: state,
      edges :: edges
      }
  | Leaf state
  deriving (Eq, Show)

type Edge state input = Moves input (State state)

type Graph state input = Graph' (State state) (Edge state input)

