{-# LANGUAGE TypeSynonymInstances, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, RecursiveDo, TypeFamilies, OverloadedStrings, RecordWildCards,UndecidableInstances, PackageImports, TemplateHaskell, RankNTypes #-}

module Graphics.Diagrams.SVG where

import Graphics.Diagrams.Core
import Graphics.Diagrams.Path
import Prelude hiding (sum,mapM_,mapM,concatMap)
import Data.List (intercalate)
import Numeric (showFFloat)
import Data.Foldable
import Data.Monoid
import Lucid.Svg

-- diaDebug msg = diaRaw $ "\n%DBG:" ++ msg ++ "\n"
class Tikz a where
  toTikz :: a -> Svg ()

instance Tikz FrozenPoint where
  toTikz pt = frozenPointElim pt $ \x y -> "(" <> showDistance x <> "," <> showDistance y <> ")"


renderPath (StraightTo p) = lR (xpart p) (ypart p)
renderPath (CurveTo c d p) = cR (xpart c) (ypart c) (xpart d) (ypart d) (xpart p) (ypart p)
renderPath Cycle = z

showDistance :: Constant -> String
showDistance x = showFFloat (Just 4) x tikzUnit
    where tikzUnit = "pt"

-- instance Tikz LineTip where
--   toTikz t = case t of
--     ToTip -> "to"
--     StealthTip -> "stealth"
--     CircleTip -> "o"
--     NoTip -> ""
--     LatexTip -> "latex"
--     ReversedTip x -> toTikz x ++ " reversed"
--     BracketTip -> "["
--     ParensTip -> "("

showDashPat :: DashPattern -> String
showDashPat xs = intercalate "," [showDistance on <> "," <> " off " <> showDistance off | (on,off) <- xs]

renderPathOptions :: PathOptions -> [Attribute]
renderPathOptions PathOptions{..} = 
    [color_ _drawColor]
    -- <> toTikz _startTip <> "-" <> toTikz _endTip <> ","
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


tikzBackend :: Backend Svg 
tikzBackend = Backend {..} where
  _tracePath options (Path start segs) = do
     path_ [d_ [mA (xpart start) (ypart start) <> renderPath segs ] ]
  -- _traceLabel :: Monad x =>
  --                  (location -> (FrozenPoint -> Tex ()) -> x ()) -> -- freezer
  --                  (forall a. Tex a -> x a) -> -- embedder
  --                  location ->
  --                  Tex () -> -- label specification
  --                  x BoxSpec
  -- _traceLabel freezer embedder point lab = do
  --      bxId <- embedder $ Tex newLabel
  --      freezer point $ \p' -> do
  --        tex $ "\\node[anchor=north west,inner sep=0] at " ++ toTikz p'
  --        fillBox bxId True $ braces $ lab
  --        tex ";\n"
  --      embedder $ getBoxFromId bxId


