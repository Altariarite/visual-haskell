-----------------------------------------------------------------------
--
-- 	Haskell: The Craft of Functional Programming, 3e
-- 	Simon Thompson
-- 	(c) Addison-Wesley, 1996-2011.
--
-- 	PicturesSVG
--
--      The Pictures functionality implemented by translation
--      SVG (Scalable Vector Graphics)
--
--      These Pictures could be rendered by conversion to ASCII art,
--      but instead are rendered into SVG, which can then be viewed in
--      a browser: google chrome does a good job.
--
-----------------------------------------------------------------------

module ShapesSVG where

import Control.Monad (liftM, liftM2)
import System.IO

-- Will be displayed in an iframe in showPic.html

data Shape
  = Circle Coords Float
  | Square Coords Float Float
  deriving (Eq, Show)

type Coords = (Float, Float)

-- Coordinates are pairs (x,y) of integers
--
--  o------> x axis
--  |
--  |
--  V
--  y axis

svgString :: Shape -> String
svgString (Circle (x, y) r) =
  "<circle cx=\""
    ++ show x
    ++ "\" cy=\""
    ++ show y
    ++ "\" r=\""
    ++ show r
    ++ "\"/>"

-- Outputting a picture.
-- The effect of this is to write the SVG code into a file
-- whose path is hardwired into the code. Could easily modify so
-- that it is an argument of the call, and indeed could also call
-- the browser to update on output.

render :: Shape -> IO ()
render shape =
  let newFile = preamble ++ svgString shape ++ postamble
   in do
        outh <- openFile "svgOut.xml" WriteMode
        hPutStrLn outh newFile
        hClose outh

-- Preamble and postamble: boilerplate XML code.

preamble =
  "<svg width=\"100%\" height=\"100%\" version=\"1.1\"\n"
    ++ "xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\">\n"
    ++ "<filter id=\"negative\">\n"
    ++ "<feColorMatrix type=\"matrix\"\n"
    ++ "values=\"-1 0  0  0  0  0 -1  0  0  0  0  0 -1  0  0  1  1  1  0  0\" />\n"
    ++ "</filter>\n"

postamble =
  "\n</svg>\n"

--
-- Examples
--

c1 = Circle (200, 200) 30