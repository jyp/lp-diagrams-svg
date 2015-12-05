{-# LANGUAGE TypeSynonymInstances, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, RecursiveDo, TypeFamilies, OverloadedStrings, RecordWildCards,UndecidableInstances, PackageImports, TemplateHaskell, RankNTypes #-}

module Graphics.Diagrams.SVG2 (renderDiagram, saveDiagram) where

import Graphics.Diagrams.Core as D
import Prelude hiding (sum,mapM_,mapM,concatMap)
import Data.List (intercalate)
import Numeric (showFFloat)
import Data.Monoid
import Graphics.Svg as S
import Control.Monad.RWS
import Linear.V2 (V2(..))
import Codec.Picture.Types (PixelRGBA8(..))
type SvgM = RWS () [Path] FrozenPoint
-- newtype SvgM a = SvgM {fromSvgM :: RWS () [Tree] FrozenPoint a} 
  
type DiagramSvg = Diagram SvgM

saveDiagram fn = saveXmlFile fn . renderDiagram

renderDiagram :: DiagramSvg a -> Document
renderDiagram d = Document {_viewBox = Nothing
                           ,_width = Nothing
                           ,_height = Nothing
                           ,_definitions = mempty
                           ,_description = "no description"
                           ,_documentLocation = "no location"
                           ,_styleRules = []
                           ,_elements = map PathTree paths}
  where (_,minPoint,paths) = runRWS (runDiagram svgBackend d) () (D.Point 1000.0 1000.0)

renderPoint (D.Point x y) = V2 x y
renderSegment (StraightTo p) = LineTo OriginAbsolute [renderPoint p]
renderSegment (D.CurveTo c d p) = S.CurveTo OriginAbsolute [(renderPoint c,renderPoint d,renderPoint p)]
renderSegment Cycle = EndPath

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

renderPathOptions :: PathOptions -> DrawAttributes
renderPathOptions PathOptions{..} = mempty
   {_strokeWidth = Last $ Just $ S.Point $ _lineWidth
   ,_strokeColor = Last $ col _drawColor
   ,S._fillColor = Last $ col _fillColor
   ,_strokeLineCap = Last $ Just $ case _lineCap of
                          RoundCap -> CapRound
                          RectCap -> CapSquare
                          ButtCap -> CapButt
    }
    -- <> toSvg _startTip <> "-" <> toSvg _endTip <> ","
    -- <> "line join=" <> (case _lineJoin of
    --                       RoundJoin -> "round"
    --                       BevelJoin -> "bevel"
    --                       MiterJoin -> "miter") <> ","
    -- <> "dash pattern=" <> showDashPat _dashPattern
    -- <> (case _decoration of
    --        Decoration [] -> ""
    --        Decoration d -> ",decorate,decoration=" ++ d)
 where col c = case c of
                 Nothing -> Just $ FillNone
                 Just "black" -> Just $ ColorRef $ PixelRGBA8 0 0 0 0
                 Just c' -> Just $ TextureRef c'

svgBackend :: Backend SvgM
svgBackend = Backend {..} where
  _tracePath _ EmptyPath = return ()
  _tracePath options (D.Path start segs) = do
    tell [S.Path (renderPathOptions options) (MoveTo OriginAbsolute [renderPoint start]:map renderSegment segs)]
  _traceLabel :: Monad x =>
                   (location -> (FrozenPoint -> SvgM ()) -> x ()) -> -- freezer
                   (forall a. SvgM a -> x a) -> -- embedder
                   location ->
                   SvgM () -> -- label specification
                   x BoxSpec
  _traceLabel freezer embedder point lab = do
    return nilBoxSpec -- TODO
  --      bxId <- embedder $ Tex newLabel
  --      freezer point $ \p' -> do
  --        tex $ "\\node[anchor=north west,inner sep=0] at " ++ toSvg p'
  --        fillBox bxId True $ braces $ lab
  --        tex ";\n"
  --      embedder $ getBoxFromId bxId


