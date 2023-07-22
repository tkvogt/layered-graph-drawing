{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word32)
import Debug.Trace (trace)
--import Diagrams.Backend.SVG.CmdLine (B, mainWith)
--import Diagrams.Prelude
import GHC.Generics
import Graph.CommonGraph (Channel, CGraph, CGraphL, EdgeType (..), NodeClass (..), EdgeClass (..), UINode)
import Graph.GraphDrawing (fromAdj, layeredGraph)
import qualified Graph.IntMap as Graph
--import Graphics.SVGFonts (textSVG)

-- several papers use this graph
testGraph :: CGraph UINodeLabel UIEdgeLabel
testGraph = fromAdj (extractNodes gr) gr
  where
    gr =
      [ (0, [4, 5, 6, 20, 29], dEdge),
        (17, [3], dEdge),
        (5, [2, 3, 4, 10, 14, 26], dEdge),
        (3, [4], dEdge),
        (26, [18], dEdge),
        (14, [10], dEdge),
        (2, [8, 23], dEdge),
        (18, [13], dEdge),
        (10, [27], dEdge),
        (2, [8, 23], dEdge),
        (23, [24, 9], dEdge),
        (24, [12, 15, 28], dEdge),
        (1, [12], dEdge),
        (12, [6, 9, 19], dEdge),
        (6, [9, 13, 21, 25, 29], dEdge),
        (29, [25], dEdge),
        (21, [22], dEdge),
        (25, [11], dEdge),
        (22, [8, 9], dEdge),
        (15, [27, 16], dEdge),
        (8, [16, 20], dEdge),
        (11, [16, 9], dEdge),
        (16, [27], dEdge),
        (27, [13, 19], dEdge),
        (13, [20], dEdge),
        (19, [9], dEdge),
        (20, [4], dEdge),
        (9, [4, 28], dEdge),
        (28, [7], dEdge)
      ]

testGraph2 :: CGraph UINodeLabel UIEdgeLabel
testGraph2 = fromAdj (extractNodes gr) gr
  where
    gr =
      [ (1, [3, 4, 21, 13], dEdge),
        (2, [3, 20], dEdge),
        (3, [4, 5, 23], dEdge),
        (4, [6], dEdge),
        (5, [7], dEdge),
        (6, [8, 16, 23], dEdge),
        (7, [9], dEdge),
        (8, [10, 11], dEdge),
        (9, [12], dEdge),
        (10, [13, 14, 15], dEdge),
        (11, [15, 16], dEdge),
        (12, [20], dEdge),
        (13, [17], dEdge),
        (14, [17, 18], dEdge),
        (16, [18, 19, 20], dEdge),
        (18, [21], dEdge),
        (19, [22], dEdge),
        (21, [23], dEdge),
        (22, [23], dEdge),
        (23, [24], dEdge),
        (24, [25], dEdge),
        (26, [27], dEdge),
        (27, [28], dEdge),
        (29, [30], dEdge),
        (30, [31], dEdge),
        (32, [31], dEdge),
        (33, [31], dEdge),
        (25, [34], dEdge),
        --           (19, [26],       dEdge),
        (23, [27], vdummyEdge),
        (27, [30], vdummyEdge)
      ]

-- A typical graph of three unconnected functions with one input and output.
-- Should be displayed vertical as options
-- Vertical lines between the function nodes give an order. If the graph grows the vertical nodes should
-- still be in one column
optionsGraph :: CGraph UINodeLabel UIEdgeLabel
optionsGraph = fromAdj (extractNodes gr) gr
  where
    gr =
      [ (1, [2], dEdge),
        (2, [3], dEdge),
        (4, [5], dEdge),
        (5, [6], dEdge),
        (7, [8], dEdge),
        (8, [9], dEdge),
        (9, [10], dEdge),
        (2, [5], vdummyEdge),
        (5, [8], vdummyEdge)
      ]

dEdge :: [UIEdgeLabel]
dEdge = [standard NormalEdge]

-- [UIEdge 2 1 "" Curly "#ff5863" "" 0 False False]

vdummyEdge :: [UIEdgeLabel]
vdummyEdge = [standard VirtualHorEdge]

-- [UIEdge 2 1 "" Curly "#ff5863" "" 0 True False]
extractNodes :: EdgeClass e => [(Word32, [Word32], [e])] -> Map.Map Word32 UINodeLabel
extractNodes adj = Map.fromList (map labelNode nodes)
  where
    nodes = (map sel1 adj) ++ (concat (map sel2 adj))
    sel1 (x, y, z) = x
    sel2 (x, y, z) = y
    labelNode n =
      ( n,
        UINodeLabel
          ( FuN
              ( FunctionNode
                  ""
                  (T.pack (show n))
                  ""
              )
          )
      )

main :: IO ()
main = return () -- mainWith (visualGraph (layeredGraph True testGraph) :: Diagram B)

------------------------------------------------------------------------------------------------

data UINodeLabel = UINodeLabel
  {
    uinode :: SpecialNode [Text]
  }
  deriving (Show)

data UIEdgeLabel = UIEdgeLabel
  { lineClass :: Text,
    strokeWidth :: Maybe Int,
    opacity :: Maybe Double,
    dashArray :: Maybe Text,
    lineColor :: Maybe Text,
    lineName :: Text,
    lineType :: Maybe LineType,
    channelNrIn :: Maybe Channel, -- arg nr x of constructor node

    -- | Me, Colleague-Name to remeber past decisions, but only people you know,
    -- or company, group
    channelNrOut :: Channel, -- function output is connected with input nr x of type node
    edgeType :: EdgeType
  }
  deriving (Show)

data LineType
  = Slanted Int -- a line consisiting of slanted sublines,
  -- (a lot of people did this, but no famous)
  | Curly -- contains a several subpaths
  deriving (Show, Generic, Eq, Ord)

data SpecialNode a
  = FuN FunctionNode
  | SubN [a]
  | DN DummyNode
  | CN ConnectionNode
  | AN ArgNode
  deriving (Eq, Ord, Show, Generic)

data DummyNode = DummyNode {dsize :: Int} deriving (Eq, Ord, Show, Generic)

data ConnectionNode = ConnectionNode {size :: Int} deriving (Eq, Ord, Show, Generic)

data FunctionNode = FunctionNode
  { functionUnique :: Text,
    functionName :: Text,
    functionType :: Text -- maybe not displayed
  }
  deriving (Eq, Ord, Generic, Show)

data ArgNode = ArgNode
  { refFunctionUnique :: Text,
    argName :: Text,
    argNr :: Int,
    isValueArg :: Bool,
    isMainFunctionArg :: Bool,
    argSelected :: Bool
  }
  deriving (Eq, Ord, Show, Generic)

instance NodeClass UINodeLabel where
  isDummy gr n = False -- maybe False isDummyLabel (Graph.lookupNode n gr)
  isConnNode gr n = maybe False isConnLabel (Graph.lookupNode n gr)
  isFunction gr n = maybe False isFuncLabel (Graph.lookupNode n gr)
  isMainArg gr n = False -- maybe False isArgLabel (Graph.lookupNode n gr)
  isSubLabel (UINodeLabel (SubN _)) = True
  isSubLabel _ = False
  isArgLabel (UINodeLabel (AN _)) = True
  isArgLabel _ = False
  subLabels (UINodeLabel (SubN as)) = length as
  connectionNode = UINodeLabel (CN (ConnectionNode 1))
  dummyNode x = UINodeLabel (DN (DummyNode x))
  nestingFeatures _ = Nothing
  updateLayer _ n = n
  verticalNumber _ = Nothing

instance EdgeClass UIEdgeLabel where
  dummyEdge chIn chOut = UIEdgeLabel "" Nothing Nothing Nothing Nothing "" Nothing chIn chOut NormalEdge
  standard edgeType =    UIEdgeLabel "" Nothing Nothing Nothing Nothing "" Nothing Nothing 0 edgeType
  edgeType (UIEdgeLabel _ _ _ _ _ _ _ _ _ e) = e
  channelNrIn (UIEdgeLabel _ _ _ _ _ _ _ channelIn channelOut _) = channelIn
  channelNrOut (UIEdgeLabel _ _ _ _ _ _ _ channelIn channelOut _) = channelOut

isDummyLabel :: UINodeLabel -> Bool
isDummyLabel (UINodeLabel (DN _)) = True
isDummyLabel _ = False

isConnLabel :: UINodeLabel -> Bool
isConnLabel (UINodeLabel (CN _)) = True
isConnLabel _ = False

isFuncLabel :: UINodeLabel -> Bool
isFuncLabel (UINodeLabel (FuN _)) = True
isFuncLabel _ = False

isArgLabel :: UINodeLabel -> Bool
isArgLabel (UINodeLabel (AN _)) = True
isArgLabel _ = False

-----------------------------------------------------------------------------------------------

-- Using diagrams to visualise the graph (this is commented out, so that you only need the quite big diagrams dependency when using this for debugging)
{-
visualGraph :: (NodeClass n, EdgeClass e) => CGraphL n e -> Diagram B
visualGraph (g, nPositions) =
  Debug.Trace.trace (show (length shortEs) ++ " : " ++ show (length longEs) ++ show (Graph.nodes testGraph, Graph.nodes g)) $
    position graphNodes # connections shortEs
      # connectionsWithoutArrow longEs
  where
    graphNodes = catMaybes (map pos (Graph.nodes g))
    pos n = fmap (\(x, y) -> (p2 ((fromIntegral x) * 2, (fromIntegral y)), vNode (fromIntegral n))) lu
      where
        lu = Map.lookup (fromIntegral n) nPositions
    es = Graph.edges g
    shortEs = filter (\(src, tgt) -> not (isConnNode g src) && not (isConnNode g tgt)) es
    longEs = filter (\(src, tgt) -> (isConnNode g src) || (isConnNode g tgt)) es
    vNode :: Int -> Diagram B
    vNode n = visualNode (n > 31) (show n) -- (isDummyNode g (fromIntegral n)) (show n)

connections es
  | (length es) < 2 = id
  | otherwise =
    foldr1 (.) $
      map
        ( \(src, tgt) ->
            connectOutside' arrowStyle2 (show src) (show tgt)
        )
        es

connectionsWithoutArrow es
  | (length es) < 2 = id
  | otherwise =
    foldr1 (.) $
      map
        ( \(src, tgt) ->
            connectOutside' arrowStyle3 (show src) (show tgt)
        )
        es

visualNode l str
  | l -- (stateLabel str <>
    =
    dummy # named str
  | otherwise = (stateLabel str <> state) # named str

text' d s = (strokeP $ textSVG s 0.7) # lw none # fc black # center # showOrigin

stateLabel = text' 0.6

arrowLabel txt size = text' size txt

state = circle 0.4 # fc silver

dummy = circle 0.05 # fc black -- mempty

shaft = arc xDir (-1 / 6 @@ turn)

shaft' = arc xDir (-2.7 / 5 @@ turn)

line = trailFromOffsets [unitX]

arrowStyle1 =
  ( with & arrowHead .~ spike & headLength .~ small
      & arrowShaft .~ shaft
  )

arrowStyle2 =
  ( with & arrowHead .~ spike & headLength .~ small
      & arrowShaft .~ line
  )

arrowStyle3 = (with & arrowHead .~ noHead & arrowShaft .~ line) -- & shaftStyle %~ dashingG [0.1, 0.05] 0)
-}
