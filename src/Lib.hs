module Lib (
    someFunc,
    demoFunc
    ) where

import DFA
import Data.Maybe
import Control.Monad

someFunc :: IO ()
someFunc = putStrLn "someFunc"

demoFunc :: IO ()
demoFunc = do
  putStr "enter state : "
  s <- getLine
  putStr "enter input : "
  t <- getLine
  print $ next s t states

next :: String -> String -> [Graph String String] -> Maybe String
next s i = join . lookup s . map (\x -> (f x, g i x))

f :: Graph' (State c) edges -> c
f = stateContents . state

g :: String -> Graph String String -> Maybe String
g i x = stateContents <$> f x yy
  where
    input (Node _ (Input ii)) inputContents = lookup inputContents ii
    input  _                  _             = Nothing
    epsilon (Node _ (Eplision stateContents)) = Just stateContents
    epsilon  _                                = Nothing
    yy = [(`input` i), epsilon]
    f x = listToMaybe . mapMaybe ($ x) 

states :: [Graph String String]
states = [
  Node (State "Start") (Eplision (State "1")),
  Node (State "1") (Input [("a",State "2"),("b",State "3")]),
  Node (State "2") (Input [("c",State "4"),("d",State "4")]),
  Node (State "3") (Input [("e",State "4"),("f",State "4")]),
  Node (State "3") (Eplision (State "Finish")),
  Leaf (State "Finish")
  ]

