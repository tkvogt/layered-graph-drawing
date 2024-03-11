{-# LANGUAGE FlexibleContexts, FlexibleInstances, OverloadedStrings, TypeFamilies, NoMonomorphismRestriction #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TupleSections #-}

module Graph.SubGraphWindows (subgraphWindows, getColumns, getRows, LayoutedSubgraph(..), ShowGraph, NestMap) where

import qualified Data.IntMap as I
import Data.List (groupBy, sortBy, (\\))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Vector.Unboxed as VU
import Debug.Trace ( trace )
import Graph.CommonGraph
  ( CGraph, CGraphL, Column, BoxId, Nesting, LayerFeatures (..), NodeClass (dummyNode, isArgLabel, updateLayer),  EdgeClass, UINode, X, Y,
    bb, childrenSeparating, layer, lb, lbb, ltb,  mid, myHead, nestingFeatures, parentsVertical, rb, rbb, rtb, tb, vHead
  )
import Graph.IntMap (nodes)
import qualified Graph.IntMap as Graph

data Span = SpanLeftBorder | SpanMiddle | SpanRightBorder | SpanOutside deriving (Eq,Show)
data SpanInOut = Outside | Inside

type Min = Int
type Max = Int

data LayoutedSubgraph n e =
     LayoutedSubgraph { outputNodeSubgraph :: [UINode],
                        subgraph :: (CGraphL n e, [[UINode]])
                      } deriving Show

type ShowGraph n e = (Enum n, Graph.ExtractNodeType n, Show n, Show e)
type NestMap = Map Nesting (Set BoxId) -- ^ boxes/subgraphs in layer
type RowNodesPartOfBox = (X, [Bool]) -- for every element in the row/column: Is it a box node?

subgraphWindows :: (NodeClass n, EdgeClass e, ShowGraph n e, VU.Unbox UINode) =>
                   (NestMap, [BoxId],[LayoutedSubgraph n e]) -> (CGraphL n e, [[UINode]]) -> (CGraphL n e, [[UINode]])
subgraphWindows (nestedGraphs, boxIds,subgraphs) ((graph, pos), layers)
  | null ns = ((graph, pos), layers)
  | otherwise = -- Debug.Trace.trace ("filledGraph " ++ show filledGraph)
    -- Debug.Trace.trace ("\n\nsubgraphWindows ") -- ++ show (graph,pos,newGraph,normalisedPos) ++"\n") -- ++ -- show newGraph ++"\n"++
  --                                      show pos ++"\n"++ show (rows,zRows,spans zRows) ++"\n"++ show (fst columns,zColumns, spans zColumns)) $
    --Debug.Trace.trace "subgraphWindows"
    ((newGraph, strip normalisedPos), layers)
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
      | isArgLabel node = --Debug.Trace.trace ("changeLayer0 " ++ show xy) $ 
                          updateLayer (changeStyle l defaultFeatures) node
      | otherwise =       --Debug.Trace.trace ("changeLayer1 " ++ show xy) $ 
                          updateLayer (changeStyle l defaultFeatures) node
      where
        l = highestLayer xy spansZRows spansZColumns
        xy = fromMaybe (0, 0, Nothing) (Map.lookup (fromIntegral n) normalisedPos)
--        _xy2 = fromMaybe (0, 0) (Map.lookup (fromIntegral root) normalisedPos)
        root = rootOf graph (fromIntegral n)

    changeLayer nestingFeats n node
      | isArgLabel node = --Debug.Trace.trace ("changeLayer2 " ++ show xy) $ 
                          updateLayer (changeStyle l (Just ((fromJust nestingFeats) {layer = sel1 l}))) node
      | otherwise =       --Debug.Trace.trace ("changeLayer3 " ++ show xy) $ 
                          updateLayer (changeStyle l nestingFeats) node
      where
        l = highestLayer xy spansZRows spansZColumns
        sel1 (x,y,z) = x
        xy = fromMaybe (0, 0, Nothing) (Map.lookup (fromIntegral n) normalisedPos)

    rootOf :: EdgeClass e => CGraph n e -> UINode -> UINode
    rootOf gr node
      | VU.null psVert = node
      | otherwise = rootOf gr (vHead 0 psVert)
      where
        psVert = parentsVertical gr node

    defaultFeatures = Just (LayerFeatures 0 Nothing Nothing)

    changeStyle :: (Nesting, Maybe BoxId, (Span, Span)) -> Maybe LayerFeatures -> Maybe LayerFeatures
    changeStyle (n, b, (SpanLeftBorder, SpanLeftBorder)) style
      = maybeReplace style n b lbb -- LeftBottomBorder
    changeStyle (n, b, (SpanLeftBorder, SpanMiddle)) style -- n >= 2    = maybeReplace style n mid
      = maybeReplace style n b bb -- BottomBorder
    changeStyle (n, b, (SpanLeftBorder, SpanRightBorder)) style -- n >= 2    = maybeReplace style n mid
      = maybeReplace style n b rbb -- RightBottomBorder
    changeStyle (n, b, (SpanMiddle, SpanLeftBorder)) style
      = maybeReplace style n b lb -- LeftBorder
    changeStyle (n, b, (SpanMiddle, SpanMiddle)) style -- No border
      = maybeReplace style n b mid
    changeStyle (n, b, (SpanMiddle, SpanRightBorder)) style -- n >= 2    = maybeReplace style n mid
      = maybeReplace style n b rb -- RightBorder
    changeStyle (n, b, (SpanRightBorder, SpanLeftBorder)) style -- n >= 4    = maybeReplace style n mid
      = maybeReplace style n b ltb -- LeftTopBorder
    changeStyle (n, b, (SpanRightBorder, SpanMiddle)) style -- n >= 4    = maybeReplace style n mid
      = maybeReplace style n b tb -- TopBorder
    changeStyle (n, b, (SpanRightBorder, SpanRightBorder)) style -- n >= 4    = maybeReplace style n mid
      = maybeReplace style n b rtb -- RightTopBorder
    changeStyle (_, _, _) style = style

    maybeReplace :: Maybe LayerFeatures -> Nesting -> Maybe BoxId -> (Nesting -> Maybe BoxId -> Maybe LayerFeatures) -> Maybe LayerFeatures
    maybeReplace (Just (LayerFeatures 0 Nothing _)) n b lf = lf n b
    maybeReplace (Just (LayerFeatures n b _)) _ _ lf = lf n b
    maybeReplace _ n b lf = Debug.Trace.trace "_" lf n b

    filledGraph = Graph.insertNodes (map fr newNodes) graph
    fr (n, nl) = (fromIntegral n, nl)
    normalisedPos :: Map UINode (X, Y, Maybe BoxId)
    normalisedPos = Map.map (\(x, y, bid) -> (x - minX, y - minY, bid)) (Map.union (Map.mapWithKey addBoxId pos) newPos)
    addBoxId k (x,y) = (x,y, bid)
      where bid = maybe Nothing boxId lf :: Maybe BoxId
            lf = maybe Nothing nestingFeatures lu :: Maybe LayerFeatures
            lu = Graph.lookupNode k graph
    newNodes = zipWith dNode [(m + 1) ..] holes
    newPos = Map.fromList (zip (map fromIntegral [(m + 1) ..]) holes)
    dNode n _ = (n, dummyNode 1)
    m | null nodePositions = maximum (nodes graph)
      | otherwise = 0

    holes :: [(Int, Int, Maybe BoxId)]
    holes = map (\(x,y) -> (x,y,Nothing))  ([(x, y) | x <- [minX .. maxX], y <- [minY .. maxY]] \\ nodePositions)

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
    rows = getRows (filledGraph, strip normalisedPos)
    columns = getColumns (filledGraph, strip normalisedPos)

    maxZCoord :: Nesting
    maxZCoord | null ns = 0
              | otherwise = maximum $ map (\(_, nl) -> zOfNode nl) ns
    zOfNode nl = maybe 0 layer (nestingFeatures nl)

    spansZRows = --Debug.Trace.trace ("zRows\n" ++ show (rows,zRows) ++ "\nspans zRows\n" ++ show (spans zRows) ++ 
                 --                   "\nzColumns\n" ++ show (fst columns, zColumns) ++ "\nspans zColumns\n" ++ show (spans zColumns)) $
                    spans zRows
    spansZColumns = spans zColumns

    zRows :: [(Nesting, [(BoxId, [RowNodesPartOfBox])])]
    zRows    = zLayers (Map.toList rows)
    zColumns = zLayers (Map.toList (fst columns))

    -- In every z-layer there can be several boxes
    -- first we draw the big boxes in the 1-layer (0-layer is the unboxed graph) and the more embedded ones on top (2..maxZCoord)
    zLayers :: [(X, [UINode])] -> [(Nesting, [(BoxId, [RowNodesPartOfBox])])]
    zLayers xs = -- Debug.Trace.trace ("nestedGraphs " ++ show nestedGraphs) $
                 map getLayer (reverse [1 .. maxZCoord])
      where
        getLayer :: Nesting -> (Nesting, [(BoxId, [RowNodesPartOfBox])])
        getLayer z = (z, maybe [] (map nodesPartOfBox) boxesOfLayerZ)
          where
            boxesOfLayerZ :: Maybe [BoxId]
            boxesOfLayerZ = fmap Set.toList (Map.lookup z nestedGraphs)
            nodesPartOfBox :: BoxId -> (BoxId, [RowNodesPartOfBox])
            nodesPartOfBox bid = (bid, map rowCol xs)
              where
                rowCol :: (X, [UINode]) -> RowNodesPartOfBox
                rowCol (x, ns) = (x, map zBoxId ns)
                zBoxId :: UINode -> Bool
                zBoxId n = boxIdOfNode == Just bid
                  where
                    lu = Graph.lookupNode n graph
                    b = maybe Nothing nestingFeatures lu
                    boxIdOfNode = maybe Nothing boxId b

    spans :: [(Nesting, [(BoxId, [RowNodesPartOfBox])])] -> Map Nesting [(BoxId, (Map X [(Min, Max)]))]
    spans ls = Map.fromList (map zSpans ls)
      where
        zSpans :: (Nesting, [(BoxId, [RowNodesPartOfBox])]) -> (Nesting, [(BoxId, Map X [(Min, Max)])])
        zSpans (z, bs) = (z, map box bs)
          where
            box :: (BoxId, [RowNodesPartOfBox]) -> (BoxId, Map X [(Min, Max)])
            box (b, rowNodes) = (b, Map.fromList (map rowsColums rowNodes))
            rowsColums :: (X, [Bool]) -> (X, [(Min, Max)])
            rowsColums (x, nodePartOfBox) = -- Debug.Trace.trace (show ("boxids", boxids, bis, map minmax bis)) 
                                            (x, map minmax fbis)
              where minmax :: [(Int, Bool)] -> (Min,Max)
                    minmax group = (fst (head group), fst (last group))
                    bis :: [[(Int, Bool)]]
                    bis = groupBy sec (zip [0..] nodePartOfBox)
                    fbis :: [[(Int, Bool)]]
                    fbis = filter (not . null) (map (filter snd) bis)
                    sec (_,a) (_,b) = a == b

--------------------------------------------------------------------------------------------

    highestLayer ::
      (X, Y, Maybe BoxId) ->
      Map Nesting [(BoxId, Map X [(Min, Max)])] ->
      Map Nesting [(BoxId, Map X [(Min, Max)])] ->
      (Nesting, Maybe BoxId, (Span, Span))
    highestLayer (x, y, bid) hrows cols = -- Debug.Trace.trace ("highestLayer " ++ show (hrows, cols)) $
                                          findFirstWindow maxZCoord
      where
        findFirstWindow 0 = (0, bid, (SpanOutside, SpanOutside))
        findFirstWindow z
          | found chead rhead = (z, bid, (chead, rhead))
          | otherwise = findFirstWindow (z - 1)
          where
            c = filter (/= SpanOutside) (map (overlappedByNeighbouringSpans (x, y) True . snd) layerCols)
            r = filter (/= SpanOutside) (map (overlappedByNeighbouringSpans (x, y) False . snd) layerRows)
            chead | null c = SpanOutside
                  | otherwise = head c
            rhead | null r = SpanOutside
                  | otherwise = head r
            layerCols = fromMaybe [] (Map.lookup z cols)
            layerRows = fromMaybe [] (Map.lookup z hrows)
            found SpanOutside _ = False
            found _ SpanOutside = False
            found _ _ = True


    -- Is there at least one neighboring row/column that includes the X/Y coordinate in its span
    overlappedByNeighbouringSpans :: (X, Y) -> Bool -> Map X [(Min, Max)] -> Span
    overlappedByNeighbouringSpans (x, y) isColumn nspans
      | isColumn = -- Debug.Trace.trace (show ("overlappedByNeighbouringSpans col",(x,y),nspans, minmax spansCol, maybe SpanOutside spanPositionColumn (minmax spansCol)))
                                     (maybe SpanOutside spanPositionColumn (minmax spansCol))
      | otherwise = -- Debug.Trace.trace (show ("overlappedByNeighbouringSpans row",(x,y),nspans, minmax spansRow, maybe SpanOutside spanPositionRow (minmax spansRow)))
                    maybe SpanOutside spanPositionRow (minmax spansRow)
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


strip :: Map k (a, b, c) -> Map k (a, b)
strip m = Map.map (\(x, y, bid) -> (x, y)) m

------------------------------------------------------------------------------------------------------------------------------

-- | To be able to jump vertically between nodes in an interactive ui
getColumns :: EdgeClass e => CGraphL n e -> (Map X [UINode], Map.Map Int [Column])
getColumns (gr, m) = (Map.fromList cols, Map.fromList (zip [0 ..] (divideTables cols)))
  where
    srt = map (sortBy sorty) . groupBy groupx . sortBy sortx
    cols = map tupleWithX ( srt (map fromIntegral (Graph.nodes gr)) )
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
        cs n = VU.length (childrenSeparating gr n) > 0
        containsSeparatingEdge = any cs

-- | To be able to jump horizontally between nodes in an interactive ui
getRows :: CGraphL n e -> Map Y [UINode]
getRows (gr, m) =
  Map.fromList $
    map tupleWithY ( srt (map fromIntegral (Graph.nodes gr)) )
  where
    srt = map (sortBy sortx) . groupBy groupy . sortBy sorty
    tupleWithY :: [UINode] -> (Y, [UINode])
    tupleWithY ls = (maybe 0 snd (Map.lookup (myHead 579 ls) m), ls)
    groupy n0 n1 = maybe 0 snd (Map.lookup n0 m) == maybe 0 snd (Map.lookup n1 m)
    sortx n0 n1 = compare (maybe 0 fst (Map.lookup n0 m)) (maybe 0 fst (Map.lookup n1 m))
    sorty n0 n1 = compare (maybe 0 snd (Map.lookup n0 m)) (maybe 0 snd (Map.lookup n1 m))
