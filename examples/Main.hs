
import Graphics.Diagrams.SVG

autoLab s i = do
  o <- labelObj s
  -- stroke "red" $ path $ polyline [o # BaseW, o # BaseE]
  -- stroke "blue" $ path $ polygon [o # NW, o # NE, o # SE, o # SW]
  autoLabel o i

testDiagram :: SvgDiagram ()
testDiagram = do
  -- draw $ path $ circle (Point 0 0) 5
  a   <- labelObj $ "ha"
  b   <- labelObj $ "b"
  a'  <- draw $ circleShape -- labelObj $ ensureMath $ "c"
  width a' === 15
  b'  <- labelObj $ "b'"
  a'' <- labelObj $ "."
  b'' <- labelObj $ "."

  -- c <- texObj $ ensureMath $ "c"
  -- Center ▸ c === MP.center [E ▸ a'', E ▸ b''] + (20 +: 0)

  let width = 70
  vdist b a === 30
  hdist a a' === width
  hdist a' a'' === width
  alignMatrix $ map (map (#Center)) $ [[a,a',a'']
                                      ,[b, b',b'']]
  autoLab "aa'" =<< arrow a a'
  autoLab "bb'" =<< arrow b b'
  autoLab "ab" . turn180 =<< arrow a b
  autoLab "a'a''" =<< arrow a' a''
  autoLab "b'b''" =<< arrow b' b''

  draw $ do
    autoLab "equal" =<< edge a'' b''
  return ()

main :: IO ()
main = renderMain testDiagram
-- fonts which fail with 'not enough bytes'
-- Cantarell, FreeSans, Asana Math
-- fonts which do work: Amiri, Roboto, DejaVu Sans, Droid Sans
