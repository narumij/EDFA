module DFAASpec where

import Test.Hspec

import DFA(Graph(..),Graph'(..),State(..),Move(..))
import DFA.A(fromA)

--import qualified DFA
import qualified DFA.A as A
--import qualified DFA.B as B

a :: [ A.Graph String String String ]
a = [
  Leaf (A.State' "?"),
  Node (A.State' "?") (Input [("i0",A.State' "??"),("i1",A.State' "???")]),
  Node (A.State' "?") (Eplision (A.State' "??"))
  ]

f :: String -> Maybe (DFA.State String)
f = (`lookup` [("?",State "A"),("??",State "B"),("???",State "C")])

f' :: String -> Maybe (DFA.State String)
f' = (`lookup` [("?",State "A"),("??",State "B")])

spec :: Spec
spec = do

  describe "DFA.A" $ do

    it "TODO: write here" $ do
      fromA f a
        `shouldBe`
        (Just [
          Leaf (State "A"),
          Node (State "A") (Input [("i0",State "B"),("i1",State "C")]),
          Node (State "A") (Eplision (State "B"))
          ] :: Maybe [Graph String String])

    it "TODO: write here" $ do
      fromA f' a
        `shouldBe`
        (Nothing :: Maybe [Graph String String])

    it "TODO: write here" $ do
      fromA (const Nothing) a
        `shouldBe`
        (Nothing :: Maybe [Graph String String])

