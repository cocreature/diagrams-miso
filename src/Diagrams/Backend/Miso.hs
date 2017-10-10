{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE NondecreasingIndentation   #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
  -- UndecidableInstances needed for ghc < 707

----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Backend.Miso
-- Copyright   :  (c) 2015 diagrams-svg team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-----------------------------------------------------------------------------

module Diagrams.Backend.Miso
  ( MisoSvg(..) -- rendering token
  , B
    -- for rendering options specific to Miso
  , Options(..)
  , sizeSpec
  , svgAttributes
  -- ,svgDefinitions, idPrefix, svgAttributes, generateDoctype
  , misoDia
  , onMouseDown
  , onMouseDown'
  , onMouseUp
  , onMouseUp'
  , onMouseMove
  , onMouseMove'
  ) where

import           Control.Lens hiding (children, transform, ( # ))
import           Control.Monad.Reader
import           Data.Aeson hiding (Options, Result)
import           Data.Bifunctor
import qualified Data.Map as M
import           Data.Tree
import           Diagrams.Core.Compile
import           Diagrams.Core.Types (Annotation (..))
import           Diagrams.Prelude hiding (Attribute, size, view, local, text, query)
import           Diagrams.TwoD.Adjust (adjustDia2D)
import           Diagrams.TwoD.Text (Text(..))
import           Miso hiding (Options, view, Result, onMouseDown, onMouseUp)
import           Miso.String (MisoString, ms)

import           Graphics.Rendering.Miso (RenderM)
import qualified Graphics.Rendering.Miso as R

nodeSvg_ :: MisoString -> [Attribute action] -> [View action] -> View action
nodeSvg_ = flip (node SVG) Nothing

-- | @MisoSvg@ is simply a token used to identify this rendering backend
--   (to aid type inference).
data MisoSvg = MisoSvg
  deriving (Show)

type B = MisoSvg

type instance V MisoSvg = V2
type instance N MisoSvg = Double

instance Monoid (Render MisoSvg V2 Double) where
  mempty = Render mempty
  Render r1 `mappend` Render r2_ = Render $ mappend r1 r2_

instance Backend MisoSvg V2 Double where
  newtype Render  MisoSvg V2 Double = Render RenderM
  type    Result  MisoSvg V2 Double = R.Element
  data    Options MisoSvg V2 Double = MisoOptions
    { _size            :: SizeSpec V2 Double   -- ^ The requested size.
    , _svgAttributes   :: R.Attrs
                          -- ^ Attributes to apply to the entire svg element.
    }

  renderRTree :: MisoSvg -> Options MisoSvg V2 Double -> RTree MisoSvg V2 Double Annotation -> Result MisoSvg V2 Double
  renderRTree _ opts rt = R.Element "svg" attrs $ runReader (rtree rt) mempty
    where
      rtree :: RTree MisoSvg V2 Double Annotation -> RenderM
      rtree (Node n rs) = case n of
        RPrim p                 -> unRender $ render MisoSvg p
        RStyle sty              -> local (<> sty) r
        _                       -> r
        where
          r = foldMap rtree rs
      V2 w h = specToSize 100 . view sizeSpec $ opts
      attrs = M.fromList [ ("width", show w)
                       , ("height", show h) ]
              <> _svgAttributes opts

  adjustDia c opts d = ( sz, t <> reflectionY, d' ) where
    (sz, t, d') = adjustDia2D sizeSpec c opts (d # reflectY)

-- | Lens onto the size of the options.
sizeSpec :: Lens' (Options MisoSvg V2 Double) (SizeSpec V2 Double)
sizeSpec f opts = f (_size opts) <&> \s -> opts { _size = s }

-- | Lens onto the svgAttributes field of the options. This field
--   is provided to supply SVG attributes to the entire diagram.
svgAttributes :: Lens' (Options MisoSvg V2 Double) R.Attrs
svgAttributes f opts =
  f (_svgAttributes opts) <&> \ds -> opts { _svgAttributes = ds }

mkWidget :: Element act -> View act
mkWidget (Element name attrs children) =
  nodeSvg_ (ms name) attrs (map mkWidget children)
mkWidget (SvgText str) = text (ms str)

unRender :: Render MisoSvg V2 Double -> RenderM
unRender (Render els) = els

instance Renderable (Path V2 Double) MisoSvg where
  render _ = Render . R.renderPath

instance Renderable (Text Double) MisoSvg where
  render _ = Render . R.renderText

instance Default (Options MisoSvg V2 Double) where
  def = MisoOptions absolute mempty

mouseEventDecoder :: Decoder (Int, Int)
mouseEventDecoder =
  Decoder
    (withObject "event" $ \o -> liftA2 (,) (o .: "clientX") (o .: "clientY"))
    mempty

query :: Monoid a => MisoString -> (a -> action) -> DiaAttr a action
query event f =
  DiaAttr
    (\dia t ->
       on
         event
         mouseEventDecoder
         (f . sample dia . transform (inv t) . fmap fromIntegral . p2))

pos :: MisoString -> (P2 Double -> action) -> DiaAttr a action
pos event f =
  DiaAttr
    (\_dia t ->
       on
         event
         mouseEventDecoder
         (f . transform (inv t) . fmap fromIntegral . p2))

data DiaAttr a action =
  DiaAttr (QDiagram MisoSvg V2 Double a -> Transformation V2 Double -> Attribute action)

onMouseDown :: (P2 Double -> action) -> DiaAttr a action
onMouseDown = pos "mousedown"

onMouseDown' :: Monoid a => (a -> action) -> DiaAttr a action
onMouseDown' = query "mousedown"

onMouseUp :: (P2 Double -> action) -> DiaAttr a action
onMouseUp = pos "mouseup"

onMouseUp' :: Monoid a => (a -> action) -> DiaAttr a action
onMouseUp' = query "mouseup"

onMouseMove :: (P2 Double -> action) -> DiaAttr a action
onMouseMove = pos "mousemove"

onMouseMove' :: Monoid a => (a -> action) -> DiaAttr a action
onMouseMove' = query "mousemove"

misoDia  :: Monoid' a => Options MisoSvg V2 Double -> QDiagram MisoSvg V2 Double a -> [DiaAttr a act] -> View act
misoDia opts dia diaAttrs =
  let (t, Element name attrs children) =
        second toMisoElement (renderDiaT MisoSvg opts dia)
  in mkWidget
       (Element name (map (\(DiaAttr f) -> f dia t) diaAttrs ++ attrs) children)

toMisoAttrs :: M.Map String String -> [Attribute act]
toMisoAttrs = map (uncurry textProp . bimap ms ms) . M.toList

data Element action
  = Element String
            [Attribute action]
            [Element action]
  | SvgText String

toMisoElement :: R.Element -> Element action
toMisoElement (R.Element name attrs children) =
  Element name (toMisoAttrs attrs) (map toMisoElement children)
toMisoElement (R.SvgText t) = SvgText t
