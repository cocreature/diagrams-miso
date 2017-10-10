{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Map as Map
import Diagrams.Prelude as D
import Diagrams.Backend.Miso
import Miso hiding (update, onMouseMove)

main :: IO ()
main =
  startApp
    (App
       (p2 (0, -1000))
       update
       viewModel
       []
       (Map.insert "mousemove" True defaultEvents)
       NoOp)

data Action
  = NoOp
  | MouseMove !(P2 Double)

update :: Action -> P2 Double -> Effect Action (P2 Double)
update NoOp p = noEff p
update (MouseMove p) _ = noEff p

viewModel :: P2 Double -> View Action
viewModel p =
  misoDia
    (def & sizeSpec .~ dims2D 500 1000)
    (mkDia p)
    [onMouseMove MouseMove]

constrain :: (InSpace v n a, Enveloped a, HasBasis v, Num n, Ord (v n)) =>
             a -> Point v n -> Point v n
constrain a p = maybe p c $ getCorners box where
  c (l,h) = max l (min h p)
  box = boundingBox a

mkDia :: P2 Double -> Diagram B
mkDia p = arr <> c <> back
  where
    arr = arrowBetween' (def & arrowHead .~ dart & arrowTail .~ quill) origin p'
    c = moveTo p' $ D.text "Hello" # fc green
    p' = constrain back p
    back = vcat [square 1000 # fc cyan, square 1000 # fc yellow]
