
import Graphics.Diagrams.SVG2
import Graphics.Diagrams

main :: IO ()
main = saveDiagram "test.svg" "Amiri" $ do
  p <- point
  draw $ path $ circle p 100
  labelObj "Hello, world"
  p .=. Point 0 0
  return ()
  -- width c === 15
