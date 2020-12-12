module DFA (
  State(..),
  Moves(..),
  Base(..),
  Node(..),
  Edge(..),
  ) where

data Moves input stateObject
  = Input [(input,stateObject)]
  | Eplision stateObject
  deriving (Eq,Show)

data State state
  = State state
  | Final
  | Start
  deriving (Eq, Show)

data Base stateObject edges
  = From {
      state :: stateObject,
      edges :: edges
      }
  | To stateObject
  deriving (Eq, Show)

type Edge state input = Moves input (State state)
type Node state input = Base (State state) (Edge state input)

