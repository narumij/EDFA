module DFA.B (
  Node(..),
  Moves(..),
  SelectEpsilon(..),
  aToB,
  ) where

import qualified DFA
import qualified DFA.A as A

data Moves input epsilon' state
    = Input  [(input, state)]
    | Epsilon state
    | Epsilon' [(epsilon', state)]
    deriving (Eq,Show)

type Edge state state' input epsilon'
  = Moves input epsilon' (A.State state state')

type Node state state' input epsilon'
  = DFA.Base (A.State state state') (Edge state state' input epsilon')

type SelectEpsilon epsilon'
  = epsilon' -> Bool

aToB :: SelectEpsilon epsilon'
    -> [Node state state' input epsilon']
    -> Maybe [A.Node state state' input]
aToB f = mapM (aToB' f)

aToB' :: SelectEpsilon epsilon'
    -> Node state state' input epsilon'
    -> Maybe (A.Node state state' input)
aToB' f n@(DFA.From s ee) = fmap (DFA.From s) (edges f ee)
aToB' _   (DFA.To s)      = Just (DFA.To s)

edges :: SelectEpsilon epsilon'
    -> Moves input epsilon' node
    -> Maybe (DFA.Moves input node)
edges _ (Input   ii)                      = Just (DFA.Input ii)
edges _ (Epsilon  e)                      = Just (DFA.Eplision e)
edges f (Epsilon' en) | hasEpsilon' f en  = Just (DFA.Eplision (epsilon' f en))
                      | otherwise         = Nothing

epsilon' :: (a -> Bool) -> [(a, c)] -> c
epsilon' f = snd . head . filter (f . fst)

hasEpsilon' :: (b1 -> Bool) -> [(b1, b2)] -> Bool
hasEpsilon' f = (==1) . length . filter (f . fst)

