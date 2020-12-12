module DFA.B (
  Graph(..),
  Moves(..),
  SelectEpsilon(..),
  fromB,
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

type Graph state state' input epsilon'
  = DFA.Graph' (A.State state state') (Edge state state' input epsilon')

type SelectEpsilon epsilon'
  = epsilon' -> Bool

fromB :: SelectEpsilon epsilon'
    -> [Graph state state' input epsilon']
    -> Maybe [A.Graph state state' input]
fromB f = mapM (aToB' f)

-- AとBを入れ替える前の名残が残っている

aToB' :: SelectEpsilon epsilon'
    -> Graph state state' input epsilon'
    -> Maybe (A.Graph state state' input)
aToB' f n@(DFA.Node s ee) = fmap (DFA.Node s) (edges f ee)
aToB' _   (DFA.Leaf s)    = Just (DFA.Leaf s)

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

