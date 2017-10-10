module Main where

import Diagrams.Prelude as D
import Diagrams.Backend.Miso
import Miso hiding (update)

main :: IO ()
main = startApp (App mempty update app [] defaultEvents NoOp)

data Action
  = ColorClick !C
  | NoOp

update :: Action -> Counts -> Effect Action Counts
update NoOp c = noEff c
update (ColorClick col) c = noEff (countC col c)

app :: Counts -> View Action
app counts =
  misoDia
    (def & sizeSpec .~ mkWidth 400)
    (mkDia counts)
    [onMouseDown' ColorClick]

mkDia :: Counts -> QDiagram B V2 Double C
mkDia (Counts r g b) =
  hcat
    [ annotate Red <$> textD r <> fc red unitCircle
    , annotate Green <$> textD g <> fc green unitCircle
    , annotate Blue <$> textD b <> fc blue unitCircle
    ]

textD :: (Show a, Monoid m) => a -> QDiagram B V2 Double m
textD = fmap (annotate mempty) . D.text . show

data C = Red | Green | Blue | Blank

data Counts = Counts Int Int Int deriving Eq

instance Semigroup C where
  (<>) = mappend

instance Monoid C where
  mempty = Blank
  mappend Blank c = c
  mappend c _ = c

instance Monoid Counts where
  mempty = Counts 0 0 0
  mappend (Counts a b c) (Counts d e f) = Counts (a+d) (b+e) (c+f)

countC :: C -> Counts -> Counts
countC Red (Counts r g b) = Counts (r+1) g b
countC Green (Counts r g b) = Counts r (g + 1) b
countC Blue (Counts r g b)= Counts r g (b+1)
countC Blank cs = cs

annotate :: Monoid c => c -> Any -> c
annotate c a | getAny a = c
annotate _ _ = mempty
