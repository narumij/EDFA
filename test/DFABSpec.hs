module DFABSpec where

import Test.Hspec

import DFA(Graph'(..),Move(..))
import DFA.A(State(..))
import DFA.B(fromB)

--import qualified DFA
--import qualified DFA.A as A
import qualified DFA.B as B

b :: [ B.Graph String String String String ]
b =  [ Node (State "0") (B.Epsilon' [ ("1",State "2"), ("3",State "4") ]) ]

spec :: Spec
spec = do

  describe "DFA.B" $ do
    it "TODO: write here" $ do
      fromB (=="1") b `shouldBe` Just [ Node (State "0") (Eplision (State "2")) ]
    it "TODO: write here" $ do
      fromB (=="3") b `shouldBe` Just [ Node (State "0") (Eplision (State "4")) ]
    it "TODO: write here" $ do
      fromB (=="2") b `shouldBe` Nothing

