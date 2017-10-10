module Main where

import Diagrams.Prelude hiding (text)
import Diagrams.Backend.Miso
import Miso hiding (update)
import Miso.String (ms)

main :: IO ()
main = startApp (App 0 update viewModel [] defaultEvents NoOp)

data Action
  = NoOp
  | Inc

dia :: QDiagram B V2 Double Any
dia = circle 100 # lc blue # lwL 2

viewModel :: Int -> View Action
viewModel i =
  div_ [] [misoDia def dia [onMouseDown' (\_ -> Inc)], (text . ms . counter) i]

update :: Action -> Int -> Effect Action Int
update NoOp i = noEff i
update Inc i = noEff (i + 1)

counter :: Int -> String
counter i = "The circle has been clicked " ++ show i ++ " times"
