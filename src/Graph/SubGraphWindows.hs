{-# LANGUAGE DeriveGeneric #-}
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
import Debug.Trace
import Graph.AjaxStructures
  ( CGraph,
    CGraphL,
    Column,
    DummyNode (..),
    LayerFeatures (..),
    SpecialNode (..),
    UINode,
    UINodeLabel (..),
    myhead,
  )
import qualified Graph.AjaxStructures as Ajax
import Graph.CommonGraph (bb, childrenSeparating, lb, lbb, ltb, mid, parentsVertical, rb, rbb, rtb, tb)
import Graph.IntMap (nodes)
import qualified Graph.IntMap as Graph

data Span = SpanLeftBorder | SpanMiddle | SpanRightBorder | SpanOutside deriving (Show)

data SpanInOut = Outside | Inside

type Layer = Int -- the nesting of the window:
-- 0 -> dummy node
-- 1 -> not part of a window
-- 2 -> first window layer

type Min = Int

type Max = Int

subgraphWindows :: VU.Unbox UINode => CGraphL -> CGraphL
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

    changeNode n (UINodeLabel node Nothing vn)
      | isArg node = UINodeLabel node (changeStyle l defaultFeatures) vn
      | otherwise = UINodeLabel node (changeStyle l defaultFeatures) vn
      where
        l = highestLayer xy (spans zRows) (spans zColumns)
        xy = fromMaybe (0, 0) (Map.lookup (fromIntegral n) normalisedPos)
    changeNode n (UINodeLabel node nestingFeats vn)
      | isArg node -- Debug.Trace.trace ("changeNode isArg" ++ show (l,xy,xy2,nestingFeats,changeStyle l nestingFeats)) $
        =
        UINodeLabel node (changeStyle l (Just ((fromJust nestingFeats) {layer = fst l}))) vn
      | otherwise -- Debug.Trace.trace ("changeNode other" ++ show (l,xy)) $
        =
        UINodeLabel node (changeStyle l nestingFeats) vn
      where
        l = highestLayer xy (spans zRows) (spans zColumns)
        xy = fromMaybe (0, 0) (Map.lookup (fromIntegral n) normalisedPos)
        _xy2 = fromMaybe (0, 0) (Map.lookup (fromIntegral root) normalisedPos)
        root = rootOf graph (fromIntegral n)

    isArg (AN _) = True
    isArg _ = False

    rootOf :: CGraph -> UINode -> UINode
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

    newNodes = zipWith dummyNode [(m + 1) ..] holes
    newPos = Map.fromList (zip (map fromIntegral [(m + 1) ..]) holes)
    dummyNode n _ = (n, UINodeLabel (DN (DummyNode 1)) Nothing Nothing)
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
    zOfNode nl = maybe 0 Ajax.layer (Ajax.nestingFeatures nl)

    zLayers :: [(X, [UINode])] -> [(Layer, [(X, [Layer])])]
    zLayers xs = map getLayer (reverse [1 .. maxZCoord])
      where
        getLayer l = (l, map zOfNodes xs)
          where
            zOfNodes (x, ns1) = (x, map zLayer ns1)
            zLayer n
              | isJust lu && (zOfNode (fromJust lu)) >= l = l
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
              | otherwise = (start, i - 1) : (minMax layers start (i + 1) Outside)

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
      | isColumn = maybe SpanOutside spanPositionColumn (minmax ((goLeft x) ++ (goRight (x + 1))))
      | otherwise = maybe SpanOutside spanPositionRow (minmax ((goLeft y) ++ (goRight (y + 1))))
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
          | otherwise = mm ++ (goLeft (p - 1))
          where
            mm = fromMaybe [] (Map.lookup p nspans)

        goRight p
          | null mm = mm
          | otherwise = mm ++ (goRight (p + 1))
          where
            mm = fromMaybe [] (Map.lookup p nspans)

type X = Int

type Y = Int

getColumns :: CGraphL -> (Map X [UINode], Map.Map Int [Column])
getColumns (gr, m) = (Map.fromList cols, Map.fromList (zip [0 ..] (divideTables cols)))
  where
    cols =
      map
        tupleWithX
        ( ( (map (sortBy sorty))
              . (groupBy groupx)
              . (sortBy sortx)
          )
            (map fromIntegral (Graph.nodes gr))
        )
    tupleWithX :: [UINode] -> (X, [UINode])
    tupleWithX ls = (maybe 0 fst (Map.lookup (myhead 504 ls) m), ls)
    groupx n0 n1 = (maybe 0 fst (Map.lookup n0 m)) == (maybe 0 fst (Map.lookup n1 m))
    sortx n0 n1 = compare (maybe 0 fst (Map.lookup n0 m)) (maybe 0 fst (Map.lookup n1 m))
    sorty n0 n1 = compare (maybe 0 snd (Map.lookup n0 m)) (maybe 0 snd (Map.lookup n1 m))

    -- There can be several graphs on the screen that are connected with separating edges
    divideTables :: [Column] -> [[Column]]
    divideTables [] = []
    divideTables layers = layersWithoutSep : (divideTables rest)
      where
        (layersWithoutSep, rest) = sumLayers ([], layers)
        sumLayers :: ([Column], [Column]) -> ([Column], [Column])
        sumLayers (s, []) = (s, [])
        sumLayers (s, l : ls)
          | containsSeparatingEdge (snd l) = (s ++ [l], ls)
          | otherwise = sumLayers (s ++ [l], ls)
        containsSeparatingEdge ns = or (map cs ns)
        cs n = VU.length (childrenSeparating gr n) > 0

getRows :: CGraphL -> Map Y [UINode]
getRows (gr, m) =
  Map.fromList $
    map
      tupleWithY
      ( ( (map (sortBy sortx))
            . (groupBy groupy)
            . (sortBy sorty)
        )
          (map fromIntegral (Graph.nodes gr))
      )
  where
    tupleWithY :: [UINode] -> (Y, [UINode])
    tupleWithY ls = (maybe 0 snd (Map.lookup (myhead 504 ls) m), ls)
    groupy n0 n1 = (maybe 0 snd (Map.lookup n0 m)) == (maybe 0 snd (Map.lookup n1 m))
    sortx n0 n1 = compare (maybe 0 fst (Map.lookup n0 m)) (maybe 0 fst (Map.lookup n1 m))
    sorty n0 n1 = compare (maybe 0 snd (Map.lookup n0 m)) (maybe 0 snd (Map.lookup n1 m))
