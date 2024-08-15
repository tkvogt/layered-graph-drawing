{-# LANGUAGE FlexibleContexts, FlexibleInstances, OverloadedStrings, TypeFamilies, NoMonomorphismRestriction #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TupleSections #-}

module Graph.SubGraphWindows (subgraphWindows, subgraphWithWindows, getColumns, getRows, ShowGraph, NestMap) where

import qualified Data.IntMap as I
import Data.List (groupBy, sortBy, (\\))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Vector.Unboxed as VU
import Debug.Trace ( trace )
import Graph.CommonGraph
  ( CGraph, CGraphL, Column, YBlocks, YBlockLines, BoxId, Nesting, Border(..), LayerFeatures (..), NodeClass (dummyNode, isArgLabel, updateLayer),  EdgeClass, UINode, X, Y,
    bb, childrenSeparating, layer, lb, lbb, ltb,  mid, myHead, nestingFeatures, parentsVertical, rb, rbb, rtb, tb, vHead
  )
import Graph.IntMap (nodes)
import qualified Graph.IntMap as Graph

data Span = SpanLeftBorder | SpanMiddle | SpanRightBorder | SpanOutside deriving (Eq,Show)
data SpanInOut = Outside | Inside

type Min = Int
type Max = Int

type ShowGraph n e = (Enum n, Graph.ExtractNodeType n, Show n, Show e)
type NestMap = Map Nesting (Set BoxId) -- ^ boxes/subgraphs in layer
type RowNodesPartOfBox = (X, [Bool]) -- for every element in the row/column: Is it a box node?

-- | Returns the boxes of the graph by examining the boxID of the cell
subgraphWindows :: (NodeClass n, EdgeClass e, ShowGraph n e, VU.Unbox UINode) =>
                   (NestMap, [BoxId]) -> (CGraphL n e, [[UINode]]) -> [(Nesting, BoxId, (Min,Max), (Min,Max))]
subgraphWindows (nestedGraphs, boxIds) ((graph, pos, yblocks), layers)
  | null ns = []
  | otherwise = concat boxes
  where
    ns = I.toList (Graph.nodeLabels graph)
    normPos = strip (normalisedPos (graph, pos))
    rows :: Map Y [UINode]
    rows    =    getRows (graph, normPos, yblocks)
    columns = getColumns (graph, normPos, yblocks)
    zRows :: [(Nesting, [(BoxId, [RowNodesPartOfBox])])]
    zRows    = zLayers graph nestedGraphs (Map.toList rows)
    zColumns = zLayers graph nestedGraphs (Map.toList (fst columns))
    spansZRows = spans zRows :: Map Nesting [(BoxId, (Map X [(Min, Max)]))]
    spansZColumns = spans zColumns :: Map Nesting [(BoxId, (Map X [(Min, Max)]))]
    boxes = zipWith f (Map.toList spansZRows) (Map.toList spansZColumns)
    f :: (Nesting, [(BoxId, (Map X [(Min, Max)]))]) -> (Nesting, [(BoxId, (Map X [(Min, Max)]))]) -> [(Nesting, BoxId, (X,X), (Y,Y))]
    f (nest0, bs0) (nest1, bs1) = bs
      where bs = zipWith boxMinMax bs0 bs1
            boxMinMax (bid0, ms0) (bid1, ms1) = (nest0, bid0, minmax (concatMap snd (Map.toList ms0)), minmax (concatMap snd (Map.toList ms1)))
            minmax mm = (minimum (map fst mm),
                         maximum (map snd mm))

-- | Adds windows by changing the border property of every cell by examining the boxID of the cell
subgraphWithWindows :: (NodeClass n, EdgeClass e, ShowGraph n e, VU.Unbox UINode) =>
                   (NestMap, [BoxId]) -> (CGraphL n e, [[UINode]]) -> (CGraphL n e, [[UINode]])
subgraphWithWindows (nestedGraphs, boxIds) ((graph, pos, yblocks), layers)
  | null ns = ((graph, pos, yblocks), layers)
  | otherwise = -- Debug.Trace.trace ("filledGraph " ++ show (filledGraph, newGraph))
    -- Debug.Trace.trace ("\n\nsubgraphWindows ") -- ++ show (graph,pos,newGraph,normalisedPos) ++"\n") -- ++ -- show newGraph ++"\n"++
  --                                      show pos ++"\n"++ show (rows,zRows,spans zRows) ++"\n"++ show (fst columns,zColumns, spans zColumns)) $
--    Debug.Trace.trace ("subgraphWindows newYblocks" ++ show (borders, newBlockNodes, yblocks, newYblocks))
    ((newGraph, normPos, newYblocks), layers)
  where
    newGraph =
      Graph.mapNodeWithKey
        changeNode
        filledGraph

    normPos = strip (normalisedPos (graph, pos))
    origPos = strip (originalPos (graph, pos))
    newYblocks = Map.toList (Map.unionWith f (Map.fromList yblocks) (Map.fromList (map extractY yblocksWithHorBorders)))

    f :: [[(UINode, X)]] -> [[(UINode, X)]] -> [[(UINode, X)]]
    f blocks0 blocks1 = foldr integrate blocks1 blocks0
      where integrate :: [(UINode, X)] -> [[(UINode, X)]] -> [[(UINode, X)]]
            integrate b0 blocks | any (oneNodePartOf b0) blocks = blocks
                                | otherwise = b0 : blocks

    oneNodePartOf :: [(UINode, X)] -> [(UINode, X)] -> Bool
    oneNodePartOf block0 block1 = any (`elem` nb1) nb0
      where nb0 = map fst block0
            nb1 = map fst block1
    extractY :: [[(UINode, (X,Y))]] -> (Y, [[(UINode, X)]])
    extractY ls = (snd (snd (head (concat ls))), map (map dropY ) ls) where dropY (n, (x,y)) = (n,x)
    -- The horizontal borders of the box are new yblocks. In javascript the borders also have to be in the same vertical position
    yblocksWithHorBorders :: [[[(UINode, (X,Y))]]]
    yblocksWithHorBorders = (map (continuousRow . sortBy sortx) . groupBy groupy . sortBy sorty) newBlockNodes
    continuousRow :: [(UINode, (X, Y))] -> [[(UINode, (X,Y))]]
    continuousRow row = continuous [head row] row
    groupy (_,(x0,y0)) (_,(x1,y1)) = y0 == y1
    sortx (_,(x0,y0)) (_,(x1,y1)) = compare x0 x1
    sorty (_,(x0,y0)) (_,(x1,y1)) = compare y0 y1

    continuous :: [(UINode, (X,Y))] -> [(UINode, (X,Y))] -> [[(UINode, (X,Y))]]
    continuous cblock (b0@(n0,(x0,y0)) : b1@(n1,(x1,y1)) : rest)
      | x0+1 == x1 = continuous (cblock ++ [b1]) (b1 : rest)
      | otherwise  = cblock : (continuous [b1] (b1 : rest))
    continuous cblock [b0@(n0,(x0,y0))] = [init cblock ++ [b0]]
    continuous cblock _ = [cblock]

    newBlockNodes :: [(UINode, (X,Y))]
    newBlockNodes = zip borders (catMaybes (map (\n -> Map.lookup (fromIntegral n) origPos) borders)) -- should all be Just values
--    borders :: [(I.Key, n)]
    borders = map (fromIntegral . fst)
                  (filter (isHorizontalBorder . fmap border . nestingFeatures . snd) (I.toList (Graph.nodeLabels newGraph)))

    isHorizontalBorder (Just (Just TopBorder)) = True
    isHorizontalBorder (Just (Just LeftTopBorder)) = True
    isHorizontalBorder (Just (Just RightTopBorder)) = True
    isHorizontalBorder (Just (Just BottomBorder)) = True
    isHorizontalBorder (Just (Just LeftBottomBorder)) = True
    isHorizontalBorder (Just (Just RightBottomBorder)) = True
    isHorizontalBorder _ = False

    changeNode :: (NodeClass n, Show n) => I.Key -> n -> n
    changeNode n node = changeLayer nf n node
      where nf = nestingFeatures node

    changeLayer :: (NodeClass n, Show n) => Maybe LayerFeatures -> I.Key -> n -> n
    changeLayer Nothing n node
      | isArgLabel node = -- Debug.Trace.trace ("changeLayer0 " ++ show (n,l,xy)) $ 
                          updateLayer (changeStyle l defaultFeatures) node
      | otherwise =       -- Debug.Trace.trace ("changeLayer1 " ++ show (n,l,xy,changeStyle l defaultFeatures)) $ 
                          updateLayer (changeStyle l defaultFeatures) node
      where
        l = highestLayer xy spansZRows spansZColumns
        xy = fromMaybe (0, 0, Nothing) (Map.lookup (fromIntegral n) (normalisedPos (graph, pos)))
--        _xy2 = fromMaybe (0, 0) (Map.lookup (fromIntegral root) normalisedPos)
--        root = rootOf graph (fromIntegral n)

    changeLayer nestingFeats n node
      | isArgLabel node = --Debug.Trace.trace ("changeLayer2 " ++ show xy) $ 
                          updateLayer (changeStyle l (Just ((fromJust nestingFeats) {layer = sel1 l}))) node
      | otherwise =       --Debug.Trace.trace ("changeLayer3 " ++ show xy) $ 
                          updateLayer (changeStyle l nestingFeats) node
      where
        l = highestLayer xy spansZRows spansZColumns
        sel1 (x,y,z) = x
        xy = fromMaybe (0, 0, Nothing) (Map.lookup (fromIntegral n) (normalisedPos (graph, pos)))

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
    rows    = getRows    (filledGraph, strip (normalisedPos (graph, pos)), yblocks)
    columns = getColumns (filledGraph, strip (normalisedPos (graph, pos)), yblocks)

    maxZCoord :: Nesting
    maxZCoord | null ns = 0
              | otherwise = maximum $ map (\(_, nl) -> zOfNode nl) ns
    zOfNode nl = maybe 0 layer (nestingFeatures nl)

    spansZRows = --Debug.Trace.trace ("zRows\n" ++ show (rows,zRows) ++ "\nspans zRows\n" ++ show (spans zRows) ++ 
                 --                   "\nzColumns\n" ++ show (fst columns, zColumns) ++ "\nspans zColumns\n" ++ show (spans zColumns)) $
                    spans zRows
    spansZColumns = spans zColumns

    zRows :: [(Nesting, [(BoxId, [RowNodesPartOfBox])])]
    zRows    = zLayers graph nestedGraphs (Map.toList rows)
    zColumns = zLayers graph nestedGraphs (Map.toList (fst columns))

--------------------------------------------------------------------------------------------

    highestLayer ::
      (X, Y, Maybe BoxId) ->
      Map Nesting [(BoxId, Map X [(Min, Max)])] ->
      Map Nesting [(BoxId, Map X [(Min, Max)])] ->
      (Nesting, Maybe BoxId, (Span, Span))
    highestLayer (x, y, bid) hrows cols = -- Debug.Trace.trace ("highestLayer " ++ show ((x,y,bid))) $
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
            layerCols = -- Debug.Trace.trace ("layerCols " ++ show (Map.lookup z cols)) $
                        fromMaybe [] (Map.lookup z cols)
            layerRows = -- Debug.Trace.trace ("layerRows " ++ show (Map.lookup z hrows))
                        fromMaybe [] (Map.lookup z hrows)
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

originalPos :: (NodeClass n, EdgeClass e, ShowGraph n e, VU.Unbox UINode) =>
                 (CGraph n e, Map UINode (X, Y)) -> Map UINode (X, Y, Maybe BoxId)
originalPos (graph, pos) = Map.map (\(x, y, bid) -> (x, y, bid)) (Map.union (Map.mapWithKey addBoxId pos) newPos)
  where
    minX | null nodePositions = 0
         | otherwise = minimum (map fst nodePositions)
    minY | null nodePositions = 0
         | otherwise = minimum (map snd nodePositions)
    maxX | null nodePositions = 0
         | otherwise = maximum (map fst nodePositions)
    maxY | null nodePositions = 0
         | otherwise = maximum (map snd nodePositions)

    nodePositions = Map.elems pos

    addBoxId k (x,y) = (x,y, bid)
      where bid = maybe Nothing boxId lf :: Maybe BoxId
            lf = maybe Nothing nestingFeatures lu :: Maybe LayerFeatures
            lu = Graph.lookupNode k graph

    newPos = Map.fromList (zip (map fromIntegral [(m + 1) ..]) holes)

    m | null nodePositions = maximum (nodes graph)
      | otherwise = 0

    holes :: [(Int, Int, Maybe BoxId)]
    holes = map (\(x,y) -> (x,y,Nothing))  ([(x, y) | x <- [minX .. maxX], y <- [minY .. maxY]] \\ nodePositions)

normalisedPos :: (NodeClass n, EdgeClass e, ShowGraph n e, VU.Unbox UINode) =>
                 (CGraph n e, Map UINode (X, Y)) -> Map UINode (X, Y, Maybe BoxId)
normalisedPos (graph, pos) = Map.map (\(x, y, bid) -> (x - minX, y - minY, bid)) (Map.union (Map.mapWithKey addBoxId pos) newPos)
  where
    minX | null nodePositions = 0
         | otherwise = minimum (map fst nodePositions)
    minY | null nodePositions = 0
         | otherwise = minimum (map snd nodePositions)
    maxX | null nodePositions = 0
         | otherwise = maximum (map fst nodePositions)
    maxY | null nodePositions = 0
         | otherwise = maximum (map snd nodePositions)

    nodePositions = Map.elems pos

    addBoxId k (x,y) = (x,y, bid)
      where bid = maybe Nothing boxId lf :: Maybe BoxId
            lf = maybe Nothing nestingFeatures lu :: Maybe LayerFeatures
            lu = Graph.lookupNode k graph

    newPos = Map.fromList (zip (map fromIntegral [(m + 1) ..]) holes)

    m | null nodePositions = maximum (nodes graph)
      | otherwise = 0

    holes :: [(Int, Int, Maybe BoxId)]
    holes = map (\(x,y) -> (x,y,Nothing))  ([(x, y) | x <- [minX .. maxX], y <- [minY .. maxY]] \\ nodePositions)


-- In every z-layer there can be several boxes
-- first we draw the big boxes in the 1-layer (0-layer is the unboxed graph) and the more embedded ones on top (2..maxZCoord)
zLayers :: (NodeClass n, EdgeClass e, ShowGraph n e, VU.Unbox UINode) =>
           CGraph n e -> NestMap -> [(X, [UINode])] -> [(Nesting, [(BoxId, [RowNodesPartOfBox])])]
zLayers graph nestedGraphs xs = map getLayer (reverse [1 .. maxZCoord])
  where
    getLayer :: Nesting -> (Nesting, [(BoxId, [RowNodesPartOfBox])])
    getLayer z = -- Debug.Trace.trace ("zLayers " ++ show (z, maybe [] (map nodesPartOfBox) boxesOfLayerZ)) $
                 (z, maybe [] (map nodesPartOfBox) boxesOfLayerZ)
      where
        boxesOfLayerZ :: Maybe [BoxId]
        boxesOfLayerZ = fmap Set.toList (Map.lookup z nestedGraphs)
        nodesPartOfBox :: BoxId -> (BoxId, [RowNodesPartOfBox])
        nodesPartOfBox bid = (bid, map rowCol xs)
          where
            rowCol :: (X, [UINode]) -> RowNodesPartOfBox
            rowCol (x, ns) = -- Debug.Trace.trace ("zLayers " ++ show (z, x, map zBoxId ns, map lu ns)) $
                             (x, map zBoxId ns)
              where
                lu n = (n, Graph.lookupNode n graph)

            zBoxId :: UINode -> Bool
            zBoxId n = -- Debug.Trace.trace ("zBoxId " ++ show (z,n,boxIdOfNode,bid,boxIdOfNode == Just bid,lu)) $
                       boxIdOfNode == Just bid
              where
                lu = Graph.lookupNode n graph
                b = maybe Nothing nestingFeatures lu
                boxIdOfNode = maybe Nothing boxId b

    maxZCoord :: Nesting
    maxZCoord | null ns = 0
              | otherwise = maximum $ map (\(_, nl) -> zOfNode nl) ns
    zOfNode nl = maybe 0 layer (nestingFeatures nl)

    ns = I.toList (Graph.nodeLabels graph)

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

-- | To be able to jump vertically between nodes in an interactive ui
getColumns :: EdgeClass e => CGraphL n e -> (Map X [UINode], Map.Map Int ([Column], YBlockLines))
getColumns (gr, m, yblocks) = -- Debug.Trace.trace ("(zip tables yblocksOfTables)" ++ show (zip tables yblocksOfTables)) $
                              (Map.fromList cols, Map.fromList (zip [0 ..] (zip tables yblocksOfTablesAdj)))
  where
    srt = map (sortBy sorty) . groupBy groupx . sortBy sortx
    cols = map tupleWithX ( srt (map fromIntegral (Graph.nodes gr)) )
    tupleWithX :: [UINode] -> (X, [UINode])
    tupleWithX ls = (maybe 0 fst (Map.lookup (myHead 78 ls) m), ls)
    groupx n0 n1 = maybe 0 fst (Map.lookup n0 m) == maybe 0 fst (Map.lookup n1 m)
    sortx n0 n1 = compare (maybe 0 fst (Map.lookup n0 m)) (maybe 0 fst (Map.lookup n1 m))
    sorty n0 n1 = compare (maybe 0 snd (Map.lookup n0 m)) (maybe 0 snd (Map.lookup n1 m))

    tables = divideTables cols

    yblocksOfTablesAdj = map adjustTable yblocksOfTables
    adjustTable :: YBlockLines -> YBlockLines
    adjustTable table = map adjustLine table
      where adjustLine :: YBlocks -> YBlocks
            adjustLine (y,line) = (y, map adjustBlock line)
            adjustBlock block = map node block
            node (n,x) = (n,x-lowestX)
    lowestX = minimum (map snd (concat (concat (concat (map (map snd) yblocksOfTables)))))
    yblocksOfTables :: [YBlockLines]
    yblocksOfTables = snd (foldr blocksOfTable (yblocks,[]) tables)

    -- There can be several graphs on the screen that are connected with separating edges
    divideTables :: [Column] -> [[Column]]
    divideTables [] = []
    divideTables layers = layersWithoutSep : divideTables rest
      where
        (layersWithoutSep, rest) = sumLayers ([], layers)

        sumLayers :: ([Column], [Column]) -> ([Column], [Column]) -- type Column = (GraphMoveX, [UINode])
        sumLayers (s, []) = (s, [])
        sumLayers (s, l : ls)
          | containsSeparatingEdge (snd l) = (s ++ [l], ls)
          | otherwise = sumLayers (s ++ [l], ls)

        containsSeparatingEdge = any cs
          where cs n = VU.length (childrenSeparating gr n) > 0

    blocksOfTable :: [Column] -> (YBlockLines, [YBlockLines]) -> (YBlockLines, [YBlockLines])
    blocksOfTable _ ([],res) = ([],res)
    blocksOfTable cols (ybs, res)
        | and (map (null . snd)  newYBlocks) = ([],res)
        | otherwise = blocksOfTable cols (reducedYBlocks, newYBlocks : res)
      where
        table = Set.fromList (concatMap snd cols)
        (reducedYBlocks, newYBlocks) = (map fst sub, map snd sub)
          where sub = map addBlockRemoveBlock ybs
        addBlockRemoveBlock :: YBlocks -> (YBlocks,YBlocks)
        addBlockRemoveBlock (y,yb) = addY (break f yb) -- | (head yb) `elem` table = (head yb, tail yb)
          where addY (yb0,yb1) = ((y,yb0),(y,yb1))
                f :: [(UINode, X)] -> Bool
                f block = or (map el block)
                el (n,_) = n `elem` table

--        (elem table . head . head) ybs

-- | To be able to jump horizontally between nodes in an interactive ui
getRows :: CGraphL n e -> Map Y [UINode]
getRows (gr, m, _) =
  Map.fromList $
    map tupleWithY ( srt (map fromIntegral (Graph.nodes gr)) )
  where
    srt = map (sortBy sortx) . groupBy groupy . sortBy sorty
    tupleWithY :: [UINode] -> (Y, [UINode])
    tupleWithY ls = (maybe 0 snd (Map.lookup (myHead 579 ls) m), ls)
    groupy n0 n1 = maybe 0 snd (Map.lookup n0 m) == maybe 0 snd (Map.lookup n1 m)
    sortx n0 n1 = compare (maybe 0 fst (Map.lookup n0 m)) (maybe 0 fst (Map.lookup n1 m))
    sorty n0 n1 = compare (maybe 0 snd (Map.lookup n0 m)) (maybe 0 snd (Map.lookup n1 m))
