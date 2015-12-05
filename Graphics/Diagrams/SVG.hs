{-# LANGUAGE TypeSynonymInstances, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, RecursiveDo, TypeFamilies, OverloadedStrings, RecordWildCards,UndecidableInstances, PackageImports, TemplateHaskell, RankNTypes #-}

module Graphics.Diagrams.SVG (renderDiagram, printDiagram) where

import Graphics.Diagrams.Core
import Prelude hiding (sum,mapM_,mapM,concatMap)
import Data.List (intercalate)
import Numeric (showFFloat)
import Data.Monoid
import Lucid.Svg
import Data.Text (pack, Text)

-- import Control.Monad.RWS
-- newtype SvgM a = SvgM {fromSvgM :: RWST () () (FrozenPoint) Svg a}

type DiagramSvg = Diagram Svg

printDiagram :: DiagramSvg a -> IO ()
printDiagram = print . renderDiagram

renderDiagram :: DiagramSvg a -> Svg a
renderDiagram d = do
  doctype_
  svg11_ $ (g_ (runDiagram svgBackend d) `with` [transform_ $ pack "translate(-100,-100)"])

renderSegment :: forall a. RealFloat a => Segment a -> Text
renderSegment (StraightTo p) = lA (xpart p) (ypart p)
renderSegment (CurveTo c d p) = cA (xpart c) (ypart c) (xpart d) (ypart d) (xpart p) (ypart p)
renderSegment Cycle = z

showDistance :: Constant -> String
showDistance x = showFFloat (Just 4) x ""

-- instance Svg LineTip where
--   toSvg t = case t of
--     ToTip -> "to"
--     StealthTip -> "stealth"
--     CircleTip -> "o"
--     NoTip -> ""
--     LatexTip -> "latex"
--     ReversedTip x -> toSvg x ++ " reversed"
--     BracketTip -> "["
--     ParensTip -> "("

showDashPat :: DashPattern -> String
showDashPat xs = intercalate "," [showDistance on <> "," <> showDistance off | (on,off) <- xs]

renderPathOptions :: PathOptions -> [Attribute]
renderPathOptions PathOptions{..} = 
    [stroke_ (col _drawColor),
     fill_ (col _fillColor)
    ]
    -- <> toSvg _startTip <> "-" <> toSvg _endTip <> ","
    -- <> "line width=" <> showDistance _lineWidth <> ","
    -- <> "line cap=" <> (case _lineCap of
    --                       RoundCap -> "round"
    --                       RectCap -> "rect"
    --                       ButtCap -> "butt") <> ","
    -- <> "line join=" <> (case _lineJoin of
    --                       RoundJoin -> "round"
    --                       BevelJoin -> "bevel"
    --                       MiterJoin -> "miter") <> ","
    -- <> "dash pattern=" <> showDashPat _dashPattern
    -- <> (case _decoration of
    --        Decoration [] -> ""
    --        Decoration d -> ",decorate,decoration=" ++ d)
  where
    col c = pack $ case c of
      Nothing -> "none"
      Just c' -> c'

svgBackend :: Backend Svg
svgBackend = Backend {..} where
  _tracePath options p = do
     path_ ([d_ (mA (xpart start) (ypart start) <> mconcat (map renderSegment segs) ) | Path start segs <- [p] ] ++ renderPathOptions options)
  _traceLabel :: Monad x =>
                   (location -> (FrozenPoint -> Svg ()) -> x ()) -> -- freezer
                   (forall a. Svg a -> x a) -> -- embedder
                   location ->
                   Svg () -> -- label specification
                   x BoxSpec
  _traceLabel freezer embedder point lab = do
    return nilBoxSpec -- TODO
  --      bxId <- embedder $ Tex newLabel
  --      freezer point $ \p' -> do
  --        tex $ "\\node[anchor=north west,inner sep=0] at " ++ toSvg p'
  --        fillBox bxId True $ braces $ lab
  --        tex ";\n"
  --      embedder $ getBoxFromId bxId


