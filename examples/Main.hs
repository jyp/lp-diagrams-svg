import Graphics.Diagrams.SVG
import Algebra.Classes
import Prelude hiding (Num(..))
-- autoLab s i = do
--   o <- label s
--   -- stroke "red" $ path $ polyline [o # BaseW, o # BaseE]
--   -- stroke "blue" $ path $ polygon [o # NW, o # NE, o # SE, o # SW]
--   autoLabelObj o i

label' x = label x x
autoLabel' x = autoLabel x x

testDiagram :: SvgDiagram ()
testDiagram = do
  noOverlap =<< label' "whatever1"
  noOverlap =<< label' "whatever2"
  -- noOverlap =<< label' "whatever3"
  -- noOverlap =<< label' "whatever4"
  a   <- label' $ "ha"

  b   <- label' $ "b"
  a'  <- draw $ circle "circleEx" -- label $ ensureMath $ "c"
  width a' === constant 15
  b'  <- label' $ "b'"
  a'' <- label' $ "."
  b'' <- label' $ "."

  let width = constant 70
  vdist b a === constant 30
  hdist a a' === width
  hdist a' a'' === width
  alignMatrix $ map (map (#Center)) $ [[a,a',a'']
                                      ,[b, b',b'']]
  noOverlap =<< autoLabel' "aa'" =<< arrow a a'
  noOverlap =<< autoLabel' "bb'" =<< arrow b b'
  noOverlap =<< autoLabel' "ab" . turn180 =<< arrow a b
  noOverlap =<< autoLabel' "a'a''" =<< arrow a' a''
  noOverlap =<< autoLabel' "b'b''" =<< arrow b' b''


  draw $ do
    autoLabel' "equal" =<< edge a'' b''
  return ()

main :: IO ()
main = renderMain testDiagram
-- fonts which fail with 'not enough bytes'
-- Cantarell, FreeSans, Asana Math
-- fonts which do work: Amiri, Roboto, DejaVu Sans, Droid Sans
