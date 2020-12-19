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
next s i = join . lookup s . map (\x -> (state x, g i x))

g :: String -> Graph String String -> Maybe String
g i x = f x yy
  where
    input (Node _ (Input ii)) inputContents = lookup inputContents ii
    input  _                  _             = Nothing
    epsilon (Node _ (Eplision stateContents)) = Just stateContents
    epsilon  _                                = Nothing
    yy = [(`input` i), epsilon]
    f x = listToMaybe . mapMaybe ($ x) 

states :: [Graph String String]
states = [
  Node "Start" (Eplision "1"),
  Node "1" (Input [("a","2"),("b","3")]),
  Node "2" (Input [("c","4"),("d","4")]),
  Node "3" (Input [("e","4"),("f","4")]),
  Node "3" (Eplision "Finish"),
  Leaf "Finish"
  ]

