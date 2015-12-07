
import Graphics.Diagrams.SVG2
import Graphics.Diagrams

main :: IO ()
main = saveDiagram "test.svg" "Amiri" $ do
  p <- point
  p .=. Point 0 0

  draw $ path $ circle p 100
  o <- labelObj "Hello, world."
  o # Center .=. Point 100 100
  return ()
  -- width c === 15
