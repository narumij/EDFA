module DFA.A (
  State(..),
  Node(..),
  SelectState(..),
  bToDFA,
  )where

import qualified DFA

data State state state'
  = State state
  | State' state'
  | Start
  | Final
  deriving (Eq,Show)

type Edge state state' input
  = DFA.Moves input (State state state')

type Node state state' input 
  = DFA.Base (State state state') (Edge state state' input)

type SelectState state state'
  = state' -> Maybe (DFA.State state)

bToDFA :: SelectState state state'
    -> [Node state state' input]
    -> Maybe [DFA.Node state input]
bToDFA f = mapM (bToDFA' f)

bToDFA'
    :: SelectState state state'
    -> Node state state' input
    -> Maybe (DFA.Node state input)
bToDFA' f (DFA.To s)      = DFA.To <$> state f s 
bToDFA' f (DFA.From s e) = DFA.From <$> state f s <*> edges f e

edges :: SelectState state state'
    -> Edge state state' input
    -> Maybe (DFA.Edge state input)
edges f (DFA.Eplision s) = DFA.Eplision <$> state f s
edges f (DFA.Input ss) = DFA.Input <$> edges' f ss

edges' :: SelectState state state' -> [(input,State state state')] -> Maybe [(input,DFA.State state)]
edges' f = mapM (edges'' f)

edges'' :: SelectState state state' -> (input, State state state') -> Maybe (input, DFA.State state)
edges'' f p = (,) (fst p) <$> (state f . snd)  p

state :: SelectState state state'
    -> State state state'
    -> Maybe (DFA.State state)
state _ Start = Just DFA.Start
state _ Final = Just DFA.Final
state _ (State s) = Just (DFA.State s)
state f (State' s') = f s'


