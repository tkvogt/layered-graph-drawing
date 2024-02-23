{-# LANGUAGE FlexibleContexts, FlexibleInstances, OverloadedStrings, TypeFamilies, NoMonomorphismRestriction #-}
{-# LANGUAGE ConstraintKinds #-}

module Graph.SubGraphWindows (subgraphWindows, getColumns, getRows, LayoutedSubgraph(..), ShowGraph) where

import qualified Data.IntMap as I
import Data.List (groupBy, sortBy, (\\))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust, fromMaybe, isJust)
import qualified Data.Vector.Unboxed as VU
import Debug.Trace ( trace )
import Graph.CommonGraph
  ( CGraph, CGraphL, Column, LayerFeatures (..), NodeClass (dummyNode, isArgLabel, updateLayer),  EdgeClass, UINode,
    bb, childrenSeparating, layer, lb, lbb, ltb,  mid, myHead, nestingFeatures, parentsVertical, rb, rbb, rtb, tb, vHead
  )
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

data LayoutedSubgraph n e =
     LayoutedSubgraph { outputNodeSubgraph :: [UINode],
                        subgraph :: (CGraphL n e, [[UINode]])
                      } deriving Show

type ShowGraph n e = (Enum n, Graph.ExtractNodeType n, Show n, Show e)

subgraphWindows :: (NodeClass n, EdgeClass e, ShowGraph n e, VU.Unbox UINode) => 
                   [LayoutedSubgraph n e] -> (CGraphL n e, [[UINode]]) -> (CGraphL n e, [[UINode]])
subgraphWindows subgraphs ((graph, pos), layers)
  | null ns = ((graph, pos), layers)
  | otherwise = -- Debug.Trace.trace ("\n\nsubgraphWindows ") -- ++ show (graph,pos,newGraph,normalisedPos) ++"\n") -- ++ -- show newGraph ++"\n"++
  --                                      show pos ++"\n"++ show (rows,zRows,spans zRows) ++"\n"++ show (fst columns,zColumns, spans zColumns)) $
    ((newGraph, normalisedPos), layers)
  where
    newGraph =
      Graph.mapNodeWithKey
        changeNode
        filledGraph

    changeNode :: (NodeClass n, Show n) => I.Key -> n -> n
    changeNode n node = changeLayer nf n node
      where nf = nestingFeatures node

    changeLayer :: (NodeClass n, Show n) => Maybe LayerFeatures -> I.Key -> n -> n
    changeLayer Nothing n node
      | isArgLabel node = updateLayer (changeStyle l defaultFeatures) node
      | otherwise =       updateLayer (changeStyle l defaultFeatures) node
      where
        l = highestLayer xy spansZRows spansZColumns
        xy = fromMaybe (0, 0) (Map.lookup (fromIntegral n) normalisedPos)
        _xy2 = fromMaybe (0, 0) (Map.lookup (fromIntegral root) normalisedPos)
        root = rootOf graph (fromIntegral n)

    changeLayer nestingFeats n node
      | isArgLabel node = updateLayer (changeStyle l (Just ((fromJust nestingFeats) {layer = fst l}))) node
      | otherwise = updateLayer (changeStyle l nestingFeats) node
      where
        l = highestLayer xy spansZRows spansZColumns
        xy = fromMaybe (0, 0) (Map.lookup (fromIntegral n) normalisedPos)

    rootOf :: EdgeClass e => CGraph n e -> UINode -> UINode
    rootOf gr node
      | VU.null psVert = node
      | otherwise = rootOf gr (vHead 0 psVert)
      where
        psVert = parentsVertical gr node

    defaultFeatures = Just (LayerFeatures 0 Nothing)

    changeStyle :: (Int, (Span, Span)) -> Maybe LayerFeatures -> Maybe LayerFeatures
    changeStyle (n, (SpanLeftBorder, SpanLeftBorder)) style
      = maybeReplace style n lbb -- LeftBottomBorder
    changeStyle (n, (SpanLeftBorder, SpanMiddle)) style -- n >= 2    = maybeReplace style n mid
      = maybeReplace style n bb -- BottomBorder
    changeStyle (n, (SpanLeftBorder, SpanRightBorder)) style -- n >= 2    = maybeReplace style n mid
      = maybeReplace style n rbb -- RightBottomBorder
    changeStyle (n, (SpanMiddle, SpanLeftBorder)) style
      = maybeReplace style n lb -- LeftBorder
    changeStyle (n, (SpanMiddle, SpanMiddle)) style -- No border
      = maybeReplace style n mid
    changeStyle (n, (SpanMiddle, SpanRightBorder)) style -- n >= 2    = maybeReplace style n mid
      = maybeReplace style n rb -- RightBorder
    changeStyle (n, (SpanRightBorder, SpanLeftBorder)) style -- n >= 4    = maybeReplace style n mid
      = maybeReplace style n ltb -- LeftTopBorder
    changeStyle (n, (SpanRightBorder, SpanMiddle)) style -- n >= 4    = maybeReplace style n mid
      = maybeReplace style n tb -- TopBorder
    changeStyle (n, (SpanRightBorder, SpanRightBorder)) style -- n >= 4    = maybeReplace style n mid
      = maybeReplace style n rtb -- RightTopBorder
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
    m | null nodePositions = maximum (nodes graph)
      | otherwise = 0

    holes :: [(Int, Int)]
    holes = [(x, y) | x <- [minX .. maxX], y <- [minY .. maxY]] \\ nodePositions

    nodePositions = Map.elems pos

    ns = I.toList (Graph.nodeLabels graph)

    minX | null nodePositions = 0
         | otherwise = minimum (map fst nodePositions)
    minY | null nodePositions = 0
         | otherwise = minimum (map snd nodePositions)
    maxX | null nodePositions = 0
         | otherwise = maximum (map fst nodePositions)
    maxY | null nodePositions = 0
         | otherwise = maximum (map snd nodePositions)

    rows :: Map Y [UINode]
    rows = getRows (filledGraph, normalisedPos)
    columns = getColumns (filledGraph, normalisedPos)

    maxZCoord | null ns = 0
              | otherwise = maximum $ map (\(_, nl) -> zOfNode nl) ns
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

    spansZRows = -- Debug.Trace.trace ("zRows\n" ++ show (rows,zRows) ++ "\nspans zRows\n" ++ show (spans zRows) ++ "\nzColumns\n" ++ show (fst columns, zColumns) ++ "\nspans zColumns\n" ++ show (spans zColumns)) $
                 spans zRows
    spansZColumns = spans zColumns

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
      Map Layer (Map Y [(Min, Max)]) ->
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
      | isColumn = maybe SpanOutside spanPositionColumn (minmax spansCol)
      | otherwise = maybe SpanOutside spanPositionRow (minmax spansRow)
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
          | p >= 0 = mm ++ goLeft (p - 1)
          | otherwise = mm
          where
            mm = fromMaybe [] (Map.lookup p nspans)

        goRight p
          | p < Map.size nspans = mm ++ goRight (p + 1)
          | otherwise = mm
          where
            mm = fromMaybe [] (Map.lookup p nspans)

{-
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
-}
        spansCol = goLeft x ++ goRight (x + 1)
        spansRow = goLeft y ++ goRight (y + 1)

------------------------------------------------------------------------------------------------------------------------------

-- | To be able to jump vertically between nodes in an interactive ui
getColumns :: EdgeClass e => CGraphL n e -> (Map X [UINode], Map.Map Int [Column])
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
    tupleWithX ls = (maybe 0 fst (Map.lookup (myHead 78 ls) m), ls)
    groupx n0 n1 = maybe 0 fst (Map.lookup n0 m) == maybe 0 fst (Map.lookup n1 m)
    sortx n0 n1 = compare (maybe 0 fst (Map.lookup n0 m)) (maybe 0 fst (Map.lookup n1 m))
    sorty n0 n1 = compare (maybe 0 snd (Map.lookup n0 m)) (maybe 0 snd (Map.lookup n1 m))

    -- There can be several graphs on the screen that are connected with separating edges
    divideTables :: [Column] -> [[Column]]
    divideTables [] = []
    divideTables layers = layersWithoutSep : divideTables rest
      where
        (layersWithoutSep, rest) = sumLayers ([], layers)
        sumLayers :: ([Column], [Column]) -> ([Column], [Column])
        sumLayers (s, []) = (s, [])
        sumLayers (s, l : ls)
          | containsSeparatingEdge (snd l) = (s ++ [l], ls)
          | otherwise = sumLayers (s ++ [l], ls)
        containsSeparatingEdge ns = any cs ns
        cs n = VU.length (childrenSeparating gr n) > 0

-- | To be able to jump horizontally between nodes in an interactive ui
getRows :: CGraphL n e -> Map Y [UINode]
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
    tupleWithY ls = (maybe 0 snd (Map.lookup (myHead 579 ls) m), ls)
    groupy n0 n1 = maybe 0 snd (Map.lookup n0 m) == maybe 0 snd (Map.lookup n1 m)
    sortx n0 n1 = compare (maybe 0 fst (Map.lookup n0 m)) (maybe 0 fst (Map.lookup n1 m))
    sorty n0 n1 = compare (maybe 0 snd (Map.lookup n0 m)) (maybe 0 snd (Map.lookup n1 m))
