module DFA.A (
  State(..),
  Graph(..),
  AnyState(..),
  fromA,
  fromA',
  )where

import qualified DFA


data State state state'
  = State state
  | State' state'
  deriving (Eq,Show)

type Edge state state' input
  = DFA.Move input (State state state')

type Graph state state' input 
  = DFA.Graph' (State state state') (Edge state state' input)

type Graph' input stateObject
  = DFA.Graph' stateObject (DFA.Move input stateObject)

type AnyState state state'
  = state' -> Maybe (DFA.State state)


recoverStateObject
  :: DFA.State state
  -> State state state'
recoverStateObject (DFA.State a) = State a

fromA :: AnyState state state'
    -> [Graph state state' input]
    -> Maybe [DFA.Graph state input]
fromA f = mapM (graph f id)

fromA' :: AnyState state state'
    -> [Graph state state' input]
    -> Maybe [Graph state state' input]
fromA' f = mapM (graph f recoverStateObject)

graph
  :: AnyState state state'
  -> (DFA.State state -> newState)
  -> Graph state state' input
  -> Maybe (Graph' input newState)
graph f g (DFA.Leaf s)   = DFA.Leaf <$> state f g s 
graph f g (DFA.Node s e) = DFA.Node <$> state f g s <*> edges f g e

edges
  :: AnyState state state'
  -> (DFA.State state -> newState)
  -> DFA.Move input (State state state')
  -> Maybe (DFA.Move input newState)
edges f g (DFA.Eplision s) = DFA.Eplision <$> state f g s
edges f g (DFA.Input ss)   = DFA.Input    <$> input f g ss

input
  :: Traversable t
  => AnyState state state' 
  -> (DFA.State state -> newState)
  -> t (input, State state state')
  -> Maybe (t (input, newState))
input f g = mapM (h f g)
  where
    h f g p = (,) (fst p) <$> (state f g . snd)  p

state
  :: AnyState state state'
  -> (DFA.State state -> newState)
  -> State state state'
  -> Maybe newState
state _ g (State s)   = g <$> Just (DFA.State s)
state f g (State' s') = g <$> f s'


