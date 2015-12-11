{-# LANGUAGE RecordWildCards #-}
module Graphics.Diagrams.SVG (module Graphics.Diagrams, renderMain, SvgDiagram) where

import Graphics.Diagrams
import System.Environment
import Graphics.Text.TrueType
import Options.Applicative
import Graphics.Diagrams.Backend.SVGTree

data Options = Options
               {
                 fontFamily :: String,
                 outputFile :: FilePath
               }

renderMain diag = do
  Options{..} <- execParser (info (helper <*> pOptions) fullDesc)
  saveDiagram outputFile fontFamily diag
  where pOptions :: Parser Options
        pOptions = Options
          <$> strOption (value "DejaVu Sans" <> long "family" <> short 'f' <> metavar "FONT" <> help "Font used for labels")
          <*> strOption (long "output-file" <> short 'o' <> metavar "OUTPUT" <> help "Output file")
