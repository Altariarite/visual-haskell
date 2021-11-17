module Main where

import qualified D3JS as D3
import qualified Data.Graph as G
import Data.Graph.Inductive (Gr, LEdge, LNode, mkGraph)
import Data.GraphViz (GraphvizParams, graphElemsToDot, graphToDot, nonClusteredParams, printDotGraph)
import Data.GraphViz.Printing (PrintDot, renderDot, toDot)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Lazy (Text, empty, pack, unpack)
import Data.Type.Equality (TestEquality)
import ShapesSVG
import System.IO

-- Graphviz needs [nodes] [edges] to create a graph
-- each node is identified by Ord a
-- A library is needed or we will have to implement the node placement algorithm by hand

-- Haskell 3e Ch14.3 Polymorphic Tree
data Tree a = Nil | Node a (Tree a) (Tree a)
  deriving (Eq, Ord, Show, Read)

sampleTree =
  Node
    'a'
    ( Node
        'b'
        (Node 'c' (Node 'e' Nil Nil) Nil)
        Nil
    )
    (Node 'd' (Node 'g' Nil Nil) (Node 'f' Nil Nil)) ::
    Tree Char

-- The following edges and nodes implementation is buggy
-- because node with the same value is not distinguished
-- turn Tree into (node,nodelabel) for graphviz
nodes :: (Ord a, Show a) => Tree a -> [(a, Text)]
nodes Nil = []
nodes (Node x ltree rtree) = (x, pack $ show x) : (nodes ltree ++ nodes rtree)

mkedge :: (Ord a) => a -> Tree a -> [(a, a, Text)]
mkedge x Nil = []
mkedge x tree = [(x, val tree, empty)]
  where
    val (Node x _ _) = x

-- (node,node,edge label)
edges :: (Ord a) => Tree a -> [(a, a, Text)]
edges Nil = []
edges (Node x Nil Nil) = []
edges (Node x ltree rtree) =
  mkedge x ltree ++ mkedge x rtree
    ++ edges ltree
    ++ edges rtree

-- edges (Node x (Node lnode lsub ))
-- can be defined in a type class? Nodes and edges to make any graph?
renderTree :: (PrintDot a, Ord a, Show a) => Tree a -> IO ()
renderTree tree =
  let dotGraph = graphElemsToDot myParams (nodes tree) (edges tree)
      dotText = printDotGraph dotGraph :: Text
   in do
        outh <- openFile "graph.dot" WriteMode
        hPutStrLn outh (unpack dotText)
        hClose outh

-- Data.Graph (Adjacency List)
sampleGraph = G.buildG (0, 4) [(0, 1), (1, 1), (0, 2), (1, 2), (2, 4), (3, 1)]

renderGraph graph =
  let nodes = [(x, empty) | x <- G.vertices graph]
      edges = [(a, b, empty) | (a, b) <- G.edges graph]
      dotGraph = graphElemsToDot myParams nodes edges
      dotText = printDotGraph dotGraph :: Text
   in do
        outh <- openFile "graph.dot" WriteMode
        hPutStrLn outh (unpack dotText)
        hClose outh

-- Scattorplot with Points
-- Could further implement 14.6 movable objects
-- Define the scatterplot
type Point = (Double, Double)

testPoints :: [Point]
testPoints = [(1, 2), (5, 10), (139, 138), (140, 150)]

myPlot points = do
  let dim = (300, 300)
  elem <- D3.box (T.pack "#myChart") dim
  D3.scatter (D3.Data2D points) elem
  D3.addFrame (300, 300) (250, 250) elem

renderScatter :: [Point] -> IO ()
renderScatter points = do
  let js = D3.reify $ myPlot points
  TIO.writeFile "generated.js" js

testGraph :: Gr Text Text
testGraph =
  mkGraph
    [ (1, empty),
      (2, empty),
      (3, empty)
    ]
    [ (1, 2, empty),
      (1, 3, empty)
    ]

myParams :: GraphvizParams n Text Text () Text
myParams = nonClusteredParams

main :: IO ()
main = renderTree sampleTree
