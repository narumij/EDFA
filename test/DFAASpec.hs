module DFAASpec where

import Test.Hspec

import DFA(Graph(..),Graph'(..),Move(..))
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

f :: String -> Maybe String
f = (`lookup` [("?", "A"),("??", "B"),("???", "C")])

f' :: String -> Maybe String
f' = (`lookup` [("?", "A"),("??", "B")])

spec :: Spec
spec = do

  describe "DFA.A" $ do

    it "TODO: write here" $ do
      fromA f a
        `shouldBe`
        (Just [
          Leaf "A",
          Node "A" (Input [("i0", "B"),("i1", "C")]),
          Node "A" (Eplision "B")
          ] :: Maybe [Graph String String])

    it "TODO: write here" $ do
      fromA f' a
        `shouldBe`
        (Nothing :: Maybe [Graph String String])

    it "TODO: write here" $ do
      fromA (const Nothing) a
        `shouldBe`
        (Nothing :: Maybe [Graph String String])

