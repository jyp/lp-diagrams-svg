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
  l1 <- label' "whatever1"
  l2 <- label' "whatever2"
  registerNonOverlap (l1#SW) (l1#NE)
  registerNonOverlap (l2#SW) (l2#NE)
  a   <- label' $ "ha"

  b   <- label' $ "b"
  a'  <- draw $ circle "circleEx" -- label $ ensureMath $ "c"
  width a' === constant 15
  b'  <- label' $ "b'"
  a'' <- label' $ "."
  b'' <- label' $ "."

  -- c <- texObj $ ensureMath $ "c"
  -- Center ▸ c === MP.center [E ▸ a'', E ▸ b''] + (20 +: 0)

  let width = constant 70
  vdist b a === constant 30
  hdist a a' === width
  hdist a' a'' === width
  alignMatrix $ map (map (#Center)) $ [[a,a',a'']
                                      ,[b, b',b'']]
  autoLabel' "aa'" =<< arrow a a'
  noOverlap =<< autoLabel' "bb'" =<< arrow b b'
  autoLabel' "ab" . turn180 =<< arrow a b
  autoLabel' "a'a''" =<< arrow a' a''
  autoLabel' "b'b''" =<< arrow b' b''


  draw $ do
    autoLabel' "equal" =<< edge a'' b''
  return ()

main :: IO ()
main = renderMain testDiagram
-- fonts which fail with 'not enough bytes'
-- Cantarell, FreeSans, Asana Math
-- fonts which do work: Amiri, Roboto, DejaVu Sans, Droid Sans
