module DFA (
  State(..),
  Moves(..),
  Base(..),
  Node(..),
  Edge(..),
  ) where

data Moves input state
  = Input [(input,state)]
  | Eplision state
  deriving (Eq,Show)

data State state
  = State state
  | Final
  | Start
  deriving (Eq, Show)

data Base state edges
  = From {
      state :: state,
      edges :: edges
      }
  | To state
  deriving (Eq, Show)

type Edge state input = Moves input (State state)
type Node state input = Base (State state) (Edge state input)

