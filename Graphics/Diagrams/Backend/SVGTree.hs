{-# LANGUAGE TypeSynonymInstances, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, RecursiveDo, TypeFamilies, OverloadedStrings, RecordWildCards,UndecidableInstances, PackageImports, TemplateHaskell, RankNTypes #-}

module Graphics.Diagrams.Backend.SVGTree (renderDiagram, saveDiagram, SvgDiagram) where

import Graphics.Diagrams.Core as D
import Prelude hiding (sum,mapM_,mapM,concatMap,Num(..),(/))
import qualified Prelude
import Graphics.Svg as S hiding (Group)
import Control.Monad.RWS
import Linear.V2 (V2(..))
import Codec.Picture.Types (PixelRGBA8(..))
import Graphics.Text.TrueType
import qualified Data.Vector.Unboxed as V
import qualified Data.Map as M
import Algebra.Classes

instance Additive a => Additive (V2 a) where
  a + b = (+) <$> a <*> b
  zero = pure zero

instance Group a => Group (V2 a) where
  a - b = (-) <$> a <*> b
  negate = fmap negate

-- TODO -- add a newtype so this does not leak to the user code.
type SvgM = RWS Font [Path] (V2 Double,V2 Double)

type SvgDiagram = Diagram String SvgM

arrowHead :: Marker
arrowHead = Marker {
  _markerDrawAttributes = mempty,
  _markerRefPoint = (Px 0,Px 0),
  _markerWidth = Nothing,
  _markerHeight = Nothing,
  _markerOrient = Just OrientationAuto,
  _markerUnits = Nothing,
  _markerViewBox = Nothing,
  _markerOverflow = Just OverflowVisible,
  _markerAspectRatio = defaultSvg,
  _markerElements =
    [PathTree $ S.Path
     mempty {_transform = Just [Scale 0.8 Nothing, Rotate 180 Nothing, Translate 12.5 0]
            ,_fillRule = Last $ Just $ FillEvenOdd
            ,_strokeColor = Last $ Just $ ColorRef $ PixelRGBA8 0 0 0 0
            ,_strokeWidth = Last $ Just $ (S.Point 1)
            ,_strokeOpacity = Just 1
            ,S._fillColor = Last $ Just $ ColorRef $ PixelRGBA8 0 0 0 0
            ,_fillOpacity = Just 1
            }
     [ MoveTo OriginAbsolute [V2 0 0], lA 5.0 (-5.0), lA (-12.5) 0.0, lA 5.0 5.0, lA 0.0 0.0, EndPath]]}
  where lA x y = LineTo OriginAbsolute [V2 x y]

saveDiagram :: FilePath -> String -> SvgDiagram () -> IO ()
saveDiagram fn fontFam d = do
  putStrLn $ "Finding font: " ++ fontFam
  Just fontFn <- findFontOfFamily fontFam (FontStyle False False)
  putStrLn $ "Loading font: " ++ fontFn
  mfont <- loadFontFile fontFn
  case mfont of
   Right font -> saveXmlFile fn $ renderDiagram font d
   Left err -> error err

renderDiagram :: Font -> SvgDiagram a -> Document
renderDiagram font d = Document
   {_viewBox = Just (lo'x,lo'y,hi'x-lo'x,hi'y-lo'y)
   ,_width = Just $ Px $ hi'x-lo'x
   ,_height = Just $ Px $ hi'y-lo'y
   ,_definitions = M.fromList [("stealth",ElementMarker arrowHead)]
   ,_description = "lp-diagrams-svg generated document"
   ,_documentLocation = "no location"
   ,_styleRules = []
   ,_elements = map PathTree paths}
  where (_,(lo,hi),paths) = runRWS (runDiagram svgBackend d) font infimum
        V2 lo'x lo'y = lo - border
        V2 hi'x hi'y = hi + border
        border = V2 5 5

ptToPx :: forall a. Field a => a -> a
ptToPx z = fromInteger 4*z / fromInteger 3

renderPoint :: FrozenPoint -> V2 Double
renderPoint (D.Point x y) = V2 (ptToPx x) (negate $ ptToPx y)

renderSegment :: Segment Coord -> PathCommand
renderSegment (StraightTo p) = LineTo OriginAbsolute [renderPoint p]
renderSegment (D.CurveTo c d p) = S.CurveTo OriginAbsolute [(renderPoint c,renderPoint d,renderPoint p)]
renderSegment Cycle = EndPath

renderTip :: LineTip -> Maybe ElementRef
renderTip t = case t of
  NoTip -> Nothing
  _ -> Just $ Ref "stealth"

renderDashPattern :: [(Double, Double)] -> Maybe [Number]
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
    ,_markerStart = Last $ renderTip _startTip
    -- _markerMid,
    ,_markerEnd = Last $ renderTip _endTip
    }
    -- <> toSvg _startTip <> "-" <> toSvg _endTip <> ","

col :: Maybe String -> Maybe Texture
col c = case c of
                 Nothing -> Just $ FillNone
                 Just "black" -> Just $ ColorRef $ PixelRGBA8 0 0 0 0
                 Just "red" -> Just $ ColorRef $ PixelRGBA8 255 0 0 100
                 Just "blue" -> Just $ ColorRef $ PixelRGBA8 0 0 255 100
                 Just c' -> Just $ TextureRef c'

lbound :: V2 Double -> V2 Double -> V2 Double
lbound (V2 x1 y1) (V2 x2 y2) = V2 (min x1 x2) (min y1 y2)
maxPt :: V2 Double
maxPt = V2 1000.0 1000.0
infimum :: (V2 Double, V2 Double)
infimum = (maxPt, negate maxPt)
union :: (V2 Double, V2 Double) -> (V2 Double, V2 Double) -> (V2 Double, V2 Double)
union (l1,h1) (l2,h2) = (lbound l1 l2, negate (lbound (negate h1) (negate h2)))
dup :: t -> (t, t)
dup x = (x,x)

pathPoints :: Path' t -> [Point' t]
pathPoints EmptyPath = []
pathPoints (D.Path start segs) = start:concat (map segPoints segs)

segPoints :: Segment t -> [Point' t]
segPoints (D.StraightTo x) = [x]
segPoints (D.CurveTo c d p) = [c,d,p]
segPoints Cycle = []

mkPairs :: forall t. [t] -> [(t, t)]
mkPairs (x:y:xs) = (x,y):mkPairs xs
mkPairs _ = []

svgBackend :: Backend String SvgM
svgBackend = Backend {..} where
  _tracePath _ EmptyPath = return ()
  _tracePath options pth@(D.Path start segs) = do
    tell [S.Path (renderPathOptions options) (MoveTo OriginAbsolute [renderPoint start]:map renderSegment segs)]
    let bx = foldr union infimum $ map dup $ map renderPoint $ pathPoints pth
    modify (union bx)
  _traceLabel :: Monad x =>
                   (location -> (FrozenPoint -> SvgM ()) -> x ()) -> -- freezer
                   (forall a. SvgM a -> x a) -> -- embedder
                   location ->
                   String -> -- label specification
                   x BoxSpec
  _traceLabel freezer embedder point lab = do
    font <- embedder ask
    let dpi = 72
        -- 144 -- 72 * 4/3
        pointSize = PointSize 10
        bbox = stringBoundingBox font dpi pointSize lab
        boxWidth = realToFrac (_xMax bbox - _xMin bbox)
        boxHeight = realToFrac (_yMax bbox - _baselineHeight bbox)
        boxDepth = realToFrac (_baselineHeight bbox - _yMin bbox)
    freezer point $ \p -> do
      let contours = getStringCurveAtPoint dpi (0,0) [(font,pointSize,lab)]
          renderPair :: (Float,Float) -> V2 Double
          renderPair (x,y) = renderPoint $ (D.Point (realToFrac x) (negate (realToFrac y) - boxHeight)) + p
      tell [S.Path textAttrs $
              concat [[MoveTo OriginAbsolute [renderPair (V.head c)]
                      ,QuadraticBezier OriginAbsolute (mkPairs $ fmap renderPair (V.toList (V.tail c)))]
                     | c <- contour, not (V.null c)]
           | contour <- contours]
    return (BoxSpec {..})


-- DrawAttributes for text rendering
textAttrs :: DrawAttributes
textAttrs = mempty {S._fillColor = Last $ col $ Just "black"}
