{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Graph.SubGraphWindows (subgraphWindows, getColumns, getRows) where

import qualified Data.IntMap as I
import Data.List (groupBy, sortBy, (\\))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust, fromMaybe, isJust)
import qualified Data.Vector.Unboxed as VU
import Debug.Trace ( trace )
import Graph.CommonGraph
  ( CGraph,
    CGraphL,
    Column,
    LayerFeatures (..),
    NodeClass (dummyNode, isArgLabel, updateLayer),
    EdgeClass,
    UINode,
    bb,
    childrenSeparating,
    layer,
    lb,
    lbb,
    ltb,
    mid,
    myhead,
    nestingFeatures,
    parentsVertical,
    rb,
    rbb,
    rtb,
    tb,
  )
import Graph.GraphDrawing (getColumns, getRows)
import Graph.IntMap (nodes)
import qualified Graph.IntMap as Graph

data Span = SpanLeftBorder | SpanMiddle | SpanRightBorder | SpanOutside deriving (Show)

data SpanInOut = Outside | Inside

type Layer = Int -- the nesting of the window:
-- 0 -> dummy node
-- 1 -> not part of a window
-- 2 -> first window layer

type X = Int

type Y = Int

type Min = Int

type Max = Int

subgraphWindows :: (EdgeClass e, NodeClass n, Show n, VU.Unbox UINode) => CGraphL n e -> CGraphL n e
subgraphWindows (graph, pos)
  | null ns = (graph, pos)
  | otherwise -- Debug.Trace.trace ("subgraphWindows "++ show (graph,pos,newGraph,normalisedPos) ++"\n") -- ++ -- show newGraph ++"\n"++
  --                                      show pos ++"\n"++ show (rows,zRows,spans zRows) ++"\n"++ show (fst columns,zColumns, spans zColumns)) $
    =
    (newGraph, normalisedPos)
  where
    newGraph =
      Graph.mapNodeWithKey
        changeNode
        filledGraph

    changeNode :: NodeClass n => I.Key -> n -> n
    changeNode n node = changeLayer nf n node
      where nf = nestingFeatures node

    changeLayer :: NodeClass n => Maybe LayerFeatures -> I.Key -> n -> n
    changeLayer Nothing n node
      | isArgLabel node = updateLayer (changeStyle l defaultFeatures) node
      | otherwise =       updateLayer (changeStyle l defaultFeatures) node
      where
        l = highestLayer xy (spans zRows) (spans zColumns)
        xy = fromMaybe (0, 0) (Map.lookup (fromIntegral n) normalisedPos)
        _xy2 = fromMaybe (0, 0) (Map.lookup (fromIntegral root) normalisedPos)
        root = rootOf graph (fromIntegral n)

    changeLayer nestingFeats n node
      | isArgLabel node = updateLayer (changeStyle l (Just ((fromJust nestingFeats) {layer = fst l}))) node
      | otherwise = updateLayer (changeStyle l nestingFeats) node
      where
        l = highestLayer xy (spans zRows) (spans zColumns)
        xy = fromMaybe (0, 0) (Map.lookup (fromIntegral n) normalisedPos)

    rootOf :: EdgeClass e => CGraph n e -> UINode -> UINode
    rootOf gr node
      | VU.null psVert = node
      | otherwise = rootOf gr (VU.head psVert)
      where
        psVert = parentsVertical gr node

    defaultFeatures = Just (LayerFeatures 0 Nothing)

    changeStyle :: (Int, (Span, Span)) -> Maybe LayerFeatures -> Maybe LayerFeatures
    changeStyle (n, (SpanLeftBorder, SpanLeftBorder)) style = maybeReplace style n lbb -- LeftBottomBorder
    changeStyle (n, (SpanLeftBorder, SpanMiddle)) style -- n >= 2    = maybeReplace style n mid
      =
      maybeReplace style n bb -- BottomBorder
    changeStyle (n, (SpanLeftBorder, SpanRightBorder)) style -- n >= 2    = maybeReplace style n mid
      =
      maybeReplace style n rbb -- RightBottomBorder
    changeStyle (n, (SpanMiddle, SpanLeftBorder)) style = maybeReplace style n lb -- LeftBorder
    changeStyle (n, (SpanMiddle, SpanMiddle)) style = maybeReplace style n mid
    changeStyle (n, (SpanMiddle, SpanRightBorder)) style -- n >= 2    = maybeReplace style n mid
      =
      maybeReplace style n rb -- RightBorder
    changeStyle (n, (SpanRightBorder, SpanLeftBorder)) style -- n >= 4    = maybeReplace style n mid
      =
      maybeReplace style n ltb -- LeftTopBorder
    changeStyle (n, (SpanRightBorder, SpanMiddle)) style -- n >= 4    = maybeReplace style n mid
      =
      maybeReplace style n tb -- TopBorder
    changeStyle (n, (SpanRightBorder, SpanRightBorder)) style -- n >= 4    = maybeReplace style n mid
      =
      maybeReplace style n rtb -- RightTopBorder
    changeStyle (_, _) style = style

    maybeReplace :: Maybe LayerFeatures -> Int -> (Int -> Maybe LayerFeatures) -> Maybe LayerFeatures
    maybeReplace (Just (LayerFeatures 0 _)) n lf = lf n
    maybeReplace (Just (LayerFeatures x _)) _ lf = lf x
    maybeReplace _ n lf = Debug.Trace.trace "_" lf n

    filledGraph = Graph.insertNodes (map fr newNodes) graph
    fr (n, nl) = (fromIntegral n, nl)
    normalisedPos = Map.map (\(x, y) -> (x - minX, y - minY)) (Map.union pos newPos)

    newNodes = zipWith dNode [(m + 1) ..] holes
    newPos = Map.fromList (zip (map fromIntegral [(m + 1) ..]) holes)
    dNode n _ = (n, dummyNode 1)
    m = maximum (nodes graph)

    holes :: [(Int, Int)]
    holes = [(x, y) | x <- [minX .. maxX], y <- [minY .. maxY]] \\ nodePositions

    nodePositions = Map.elems pos

    ns = I.toList (Graph.nodeLabels graph)

    minX = minimum (map fst nodePositions)
    minY = minimum (map snd nodePositions)
    maxX = maximum (map fst nodePositions)
    maxY = maximum (map snd nodePositions)

    rows :: Map Y [UINode]
    rows = getRows (filledGraph, normalisedPos)
    columns = getColumns (filledGraph, normalisedPos)

    maxZCoord = maximum $ map (\(_, nl) -> zOfNode nl) ns
    zOfNode nl = maybe 0 layer (nestingFeatures nl)

    zLayers :: [(X, [UINode])] -> [(Layer, [(X, [Layer])])]
    zLayers xs = map getLayer (reverse [1 .. maxZCoord])
      where
        getLayer l = (l, map zOfNodes xs)
          where
            zOfNodes (x, ns1) = (x, map zLayer ns1)
            zLayer n
              | isJust lu && zOfNode (fromJust lu) >= l = l
              | otherwise = 0
              where
                lu = Graph.lookupNode n graph

    zRows :: [(Layer, [(X, [Layer])])]
    zRows = zLayers (Map.toList rows)
    zColumns = zLayers (Map.toList (fst columns))

    spans :: [(Layer, [(X, [Layer])])] -> Map Layer (Map X [(Min, Max)])
    spans ls = Map.fromList (map zSpans ls)
      where
        zSpans (z, rs) = (z, Map.fromList (map rowsColums rs))
          where
            rowsColums (x, layers) = (x, minMax layers 0 0 Outside)

            minMax :: [Layer] -> Int -> Int -> SpanInOut -> [(Min, Max)]
            minMax [] start i Inside = [(start, i - 1)]
            minMax [] _ _ _ = []
            minMax (l : layers) start i Outside
              | l == z = minMax layers i (i + 1) Inside
              | otherwise = minMax layers start (i + 1) Outside
            minMax (l : layers) start i Inside
              | l == z = minMax layers start (i + 1) Inside
              | otherwise = (start, i - 1) : minMax layers start (i + 1) Outside

    highestLayer ::
      (X, Y) ->
      Map Layer (Map X [(Min, Max)]) ->
      Map Layer (Map X [(Min, Max)]) ->
      (Layer, (Span, Span))
    highestLayer (x, y) hrows cols = findFirstWindow maxZCoord
      where
        findFirstWindow 0 = (0, (SpanOutside, SpanOutside))
        findFirstWindow z
          | found c r = (z, (c, r))
          | otherwise = findFirstWindow (z - 1)
          where
            c = maybe SpanOutside (overlappedByNeighbouringSpans (x, y) True) (Map.lookup z cols)
            r = maybe SpanOutside (overlappedByNeighbouringSpans (x, y) False) (Map.lookup z hrows)
            found SpanOutside _ = False
            found _ SpanOutside = False
            found _ _ = True

    -- Is there at least one neighboring row/column that includes the X/Y coordinate in its span
    overlappedByNeighbouringSpans :: (X, Y) -> Bool -> Map X [(Min, Max)] -> Span
    overlappedByNeighbouringSpans (x, y) isColumn nspans
      | isColumn = maybe SpanOutside spanPositionColumn (minmax (goLeft x ++ goRight (x + 1)))
      | otherwise = maybe SpanOutside spanPositionRow (minmax (goLeft y ++ goRight (y + 1)))
      where
        spanPositionColumn (smin, smax)
          | y == smin = SpanRightBorder
          | y == smax = SpanLeftBorder
          | y > smin && y < smax = SpanMiddle
          | otherwise = SpanOutside
        spanPositionRow (smin, smax)
          | x == smin = SpanLeftBorder
          | x == smax = SpanRightBorder
          | x > smin && x < smax = SpanMiddle
          | otherwise = SpanOutside
        minmax xs
          | null xs = Nothing
          | otherwise = Just (minimum (map fst xs), maximum (map snd xs))

        goLeft p
          | null mm = mm
          | otherwise = mm ++ goLeft (p - 1)
          where
            mm = fromMaybe [] (Map.lookup p nspans)

        goRight p
          | null mm = mm
          | otherwise = mm ++ goRight (p + 1)
          where
            mm = fromMaybe [] (Map.lookup p nspans)
