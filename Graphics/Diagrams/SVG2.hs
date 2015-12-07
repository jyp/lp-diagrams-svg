{-# LANGUAGE TypeSynonymInstances, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, RecursiveDo, TypeFamilies, OverloadedStrings, RecordWildCards,UndecidableInstances, PackageImports, TemplateHaskell, RankNTypes #-}

module Graphics.Diagrams.SVG2 (renderDiagram, saveDiagram) where

import Graphics.Diagrams.Core as D
import Prelude hiding (sum,mapM_,mapM,concatMap)
import Data.List (intercalate)
import Numeric (showFFloat)
import Data.Monoid
import Graphics.Svg as S
import Control.Monad.RWS
import Control.Monad (when)
import Linear.V2 (V2(..))
import Codec.Picture.Types (PixelRGBA8(..))
import Graphics.Text.TrueType
import qualified Data.Vector.Unboxed as V

type SvgM = RWS Font [Path] (FrozenPoint,FrozenPoint)
-- newtype SvgM a = SvgM {fromSvgM :: RWS () [Tree] FrozenPoint a} 

type DiagramSvg = Diagram String SvgM

saveDiagram :: FilePath -> String -> DiagramSvg () -> IO ()
saveDiagram fn fontFam d = do
  putStrLn $ "Finding font: " ++ fontFam
  Just fontFn <- findFontOfFamily fontFam (FontStyle False False)
  putStrLn $ "Loading font: " ++ fontFn
  mfont <- loadFontFile fontFn
  case mfont of
   Right font -> saveXmlFile fn $ renderDiagram font d
   Left err -> error err

-- renderDiagram :: DiagramSvg a -> Document
renderDiagram font d = Document
   {_viewBox = Just (xpart lo',ypart lo',xpart sz,ypart sz)
   ,_width = Nothing
   ,_height = Nothing
   ,_definitions = mempty
   ,_description = "no description"
   ,_documentLocation = "no location"
   ,_styleRules = []
   ,_elements = map PathTree paths}
  where (_,(lo,hi),paths) = runRWS (runDiagram svgBackend d) font infimum
        lo' = fmap floor lo
        hi' = fmap ceiling hi
        sz = hi' - lo'
ptToPx z = 4*z / 3

renderPoint (D.Point x y) = V2 (ptToPx x) (ptToPx y)

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

renderDashPattern [] = Nothing
renderDashPattern xs = Just $ concat [[S.Point on, S.Point off] | (on,off) <- xs]

renderPathOptions :: PathOptions -> DrawAttributes
renderPathOptions PathOptions{..} = mempty
   {_strokeWidth = Last $ Just $ S.Point $ _lineWidth
   ,_strokeColor = Last $ col _drawColor
   ,S._fillColor = Last $ col _fillColor
   ,_strokeLineCap = Last $ Just $ case _lineCap of
                          RoundCap -> CapRound
                          RectCap -> CapSquare
                          ButtCap -> CapButt
                          
    -- _strokeOpacity,
   ,_strokeLineJoin = case _lineJoin of
                       RoundJoin -> Last $ Just $ JoinRound
                       BevelJoin -> Last $ Just $ JoinBevel
                       MiterJoin -> Last $ Just $ JoinMiter
    -- _strokeMiterLimit,
    -- _fillOpacity,
    -- _groupOpacity,
    -- _transform, _fillRule,
    -- _maskRef, _clipPathRef,
    -- _clipRule, _attrClass,
    -- _attrId, _strokeOffset,
   ,_strokeDashArray = Last $ renderDashPattern _dashPattern
    -- _fontSize, _fontFamily,
    -- _fontStyle,
    -- _textAnchor,
    -- _markerStart,
    -- _markerMid, _markerEnd
    }
    -- <> toSvg _startTip <> "-" <> toSvg _endTip <> ","
col c = case c of
                 Nothing -> Just $ FillNone
                 Just "black" -> Just $ ColorRef $ PixelRGBA8 0 0 0 0
                 Just c' -> Just $ TextureRef c'

renderPair :: (Float,Float) -> V2 Double
renderPair (x,y) = V2 (realToFrac x) (realToFrac y)

lbound (D.Point x1 y1) (D.Point x2 y2) = D.Point (min x1 x2) (min y1 y2)
maxPt = D.Point 1000.0 1000.0
infimum = (maxPt, negate maxPt)
union (l1,h1) (l2,h2) = (lbound l1 l2, negate (lbound (negate h1) (negate h2)))
dup x = (x,x)

pathPoints EmptyPath = []
pathPoints (D.Path start segs) = start:concat (map segPoints segs)

segPoints (D.StraightTo x) = [x]
segPoints (D.CurveTo c d p) = [c,d,p]
segPoints Cycle = []

mkPairs (x:y:xs) = (x,y):mkPairs xs
mkPairs _ = []
svgBackend :: Backend String SvgM
svgBackend = Backend {..} where
  _tracePath _ EmptyPath = return ()
  _tracePath options pth@(D.Path start segs) = do
    tell [S.Path (renderPathOptions options) (MoveTo OriginAbsolute [renderPoint start]:map renderSegment segs)]
    let bx = foldr union infimum $ map dup (pathPoints pth)
    modify (union bx)
  _traceLabel :: Monad x =>
                   (location -> (FrozenPoint -> SvgM ()) -> x ()) -> -- freezer
                   (forall a. SvgM a -> x a) -> -- embedder
                   location ->
                   String -> -- label specification
                   x BoxSpec
  _traceLabel freezer embedder point lab = do
    font <- embedder ask
    let dpi = 144 -- 72 * 4/3
        pointSize = PointSize 10
        bbox = stringBoundingBox font dpi pointSize lab
        boxHeight = realToFrac (_baselineHeight bbox - _yMin bbox)
        boxWidth = realToFrac (_xMax bbox - _xMin bbox)
        boxDepth = realToFrac (_yMax bbox - _baselineHeight bbox)
    freezer point $ \p -> do
      let contours = getStringCurveAtPoint dpi (realToFrac $ xpart p, realToFrac $ ypart p) [(font,pointSize,lab)]
      tell [S.Path textAttrs $
              concat [[MoveTo OriginAbsolute [renderPair (V.head c)]
                      ,QuadraticBezier OriginAbsolute (mkPairs $ fmap renderPair (V.toList (V.tail c)))]
                     | c <- contour, not (V.null c)]
           | contour <- contours]
    return (BoxSpec {..})
    return nilBoxSpec -- FIXME


textAttrs = mempty {S._fillColor = Last $ col $ Just "black"}
