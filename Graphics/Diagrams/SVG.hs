{-# LANGUAGE TypeSynonymInstances, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, RecursiveDo, TypeFamilies, OverloadedStrings, RecordWildCards,UndecidableInstances, PackageImports, TemplateHaskell, RankNTypes #-}

module Graphics.Diagrams.SVG (renderDiagram, printDiagram) where

import Graphics.Diagrams.Core
import Prelude hiding (sum,mapM_,mapM,concatMap)
import Data.List (intercalate)
import Numeric (showFFloat)
import Data.Foldable
import Data.Monoid
import Lucid.Svg
import Data.Text (pack, Text)

-- renderPoint pt = frozenPointElim pt $ \x y -> "(" <> showDistance x <> "," <> showDistance y <> ")"

printDiagram :: Diagram Svg a -> IO ()
printDiagram = print . renderDiagram

renderDiagram :: Diagram Svg a -> Svg a
renderDiagram d = do
  doctype_
  with (svg11_ (runDiagram svgBackend d)) []


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
showDashPat xs = intercalate "," [showDistance on <> "," <> " off " <> showDistance off | (on,off) <- xs]

renderPathOptions :: PathOptions -> [Attribute]
renderPathOptions PathOptions{..} = 
    [stroke_ (pack c) | Just c <- [_drawColor] ]
    -- <> toSvg _startTip <> "-" <> toSvg _endTip <> ","
    -- <> col "fill" _fillColor
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


