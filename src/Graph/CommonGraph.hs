{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Graph.CommonGraph where

import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.IntMap as I
import Data.List (group, sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, isJust)
import qualified Data.Vector.Unboxed as VU
import Data.Word (Word32, Word8)
import Debug.Trace (trace)
import GHC.Generics (Generic)
import Graph.IntMap
  ( Edge8 (..),
    EdgeAttribute (..),
    ExtractNodeType (..),
    Graph (..),
    adjacentNodesByAttr,
  )

-- | Word32 is used for the node because graph drawing is most likely used in a browser with javascript and the ints there have 32 bits, and this is more than enough
type UINode = Word32

-- | A shorthand for multiple edges
type CGraph n e = Graph n [e]

-- | Layouted Graph, assign a (x,y) position to every node
type CGraphL n e = (Graph n [e], Map UINode (X, Y))

type X = Int
type Y = Int

-- | Nodes could be grouped into lists. But as a lof of algorithms walk through the graph, 
--   it is more convenient to see for example if a node is connected vertically than to see if it is part of a list of vertically grouped nodes.
--   This is of course a matter of taste and there probably good arguments to put nodes in lists
data EdgeType
  = NormalEdge
  | VerticalEdge -- ^When having options, they appear continuously in one column. We mark this in the graph with vertical edges from the first option to the second and so on
  | VirtualHorEdge -- ^Virtual edges are not displayed but used to put several graphs in a row for layouting and navigation with the keyboard
  | SeparatingEdge -- ^To connect graph components that are separate
  deriving (Show, Generic, Eq, Ord)

type GraphMoveX = Int

type Column = (GraphMoveX, [UINode])

-- | A type class for the node type, so that an individual node type can be used
--   Some functions had to be introduced that are special for the original purpose. The main reason for not using grapviz was that it became clear that a binding to graphviz does not allow to adjust algorithms easily
class NodeClass n where
  isDummy :: EdgeClass e => CGraph n e -> UINode -> Bool
  isCase :: EdgeClass e => CGraph n e -> UINode -> Bool
  isConnNode :: EdgeClass e => CGraph n e -> UINode -> Bool
  isFunction :: EdgeClass e => CGraph n e -> UINode -> Bool -- ^ This special for displaying function networks
  isMainArg :: EdgeClass e => CGraph n e -> UINode -> Bool -- ^ This special for displaying function networks
  isSubLabel :: n -> Bool
  isArgLabel :: n -> Bool -- ^ This special for displaying function networks
  subLabels :: n -> Int
  connectionNode :: n
  dummyNode :: Int -> n -- Depth -> n
  nestingFeatures :: n -> Maybe LayerFeatures
  updateLayer :: Maybe LayerFeatures -> n -> n
  verticalNumber :: n -> Maybe Int -- we want to keep the order of vertically connected Nodes,

type ChannelNrIn = Maybe Channel

type ChannelNrOut = Channel

-- | A channel (or port) is used if a node has several subfields that are connected separately
--   For example the nth type of a type node
type Channel = Int

-- | Edges can are also be implemented individually
class EdgeClass e where
  dummyEdge :: ChannelNrIn -> ChannelNrOut -> e
  standard :: EdgeType -> e
  edgeType :: e -> EdgeType
  channelNrIn :: e -> ChannelNrIn
  channelNrOut :: e -> ChannelNrOut

------------------------------------------------------------------------------------------------------
-- * Grouping edges into classes with non overlapping bits, 
--
-- $bits
--
-- For example vertBit = 00000001 = 1, virtBit = 00000010 = 2, sepBit = 00000100 =4, ...
--   Up to 8 bits. This was used to have a superfast lookup with an intmap. Maybe a normal Map with a key (UINode,Word32) would have been easier, with Word32 being the edge type. But this is faster.

vertBit :: Word8
vertBit = 0x1

virtBit :: Word8
virtBit = 0x2

sepBit :: Word8
sepBit = 0x4

instance EdgeClass e => EdgeAttribute [e] where -- Why can two nodes be connected with more than one edge?
-- To connect one function with several input types that are part of one type node
  fastEdgeAttr (e : _) = f (edgeType e)
    where
      f VerticalEdge = vertBit
      f VirtualHorEdge = virtBit
      f _ = 0
  fastEdgeAttr _ = 0
  edgeFromAttr =
    Map.fromList
      [ (vertBit, [standard VerticalEdge]),
        (virtBit, [standard VirtualHorEdge]),
        (0, [standard NormalEdge])
      ]

--  show_e (Just [UIEdgeLabel standard Nothing 0 e]) = show e
  show_e _ = "no Edge"
  bases _ = [Edge8 0, Edge8 vertBit, Edge8 virtBit]

------------------------------------------------------------------------------------------------------
-- * Querying nodes that are connected by a certain type of edge
--   

-- | All children that are connected but without the vertically connected nodes
childrenNoVertical :: EdgeClass e => Graph n [e] -> Word32 -> VU.Vector Word32
childrenNoVertical gr n =
  adjacentNodesByAttr gr True n (Edge8 virtBit)
    VU.++ adjacentNodesByAttr gr True n (Edge8 0)

-- | All parents that are connected but without the vertically connected nodes
parentsNoVertical :: EdgeClass e => Graph n [e] -> Word32 -> VU.Vector Word32
parentsNoVertical gr n =
  adjacentNodesByAttr gr False n (Edge8 virtBit)
    VU.++ adjacentNodesByAttr gr False n (Edge8 0)

-- | All parents that are connected but without the virtual connected nodes
parentsNoVirtual :: EdgeClass e => CGraph n e -> Word32 -> VU.Vector Word32
parentsNoVirtual gr n =
  (adjacentNodesByAttr gr False n (Edge8 vertBit))
    VU.++ (adjacentNodesByAttr gr False n (Edge8 0))

-- | Children that are connected vertically
childrenVertical :: EdgeClass e => Graph n [e] -> Word32 -> VU.Vector Word32
childrenVertical gr n = adjacentNodesByAttr gr True n (Edge8 vertBit)

-- | Parents that are connected vertically
parentsVertical :: EdgeClass e => Graph n [e] -> Word32 -> VU.Vector Word32
parentsVertical gr n = adjacentNodesByAttr gr False n (Edge8 vertBit)

-- | Children that are connected with a separating edge
childrenSeparating :: EdgeClass e => CGraph n e -> Word32 -> VU.Vector Word32
childrenSeparating gr n = adjacentNodesByAttr gr True n (Edge8 sepBit)

-- | Find all vertically connected nodes, by exploring incoming and outgoing vertical edges 
verticallyConnectedNodes :: EdgeClass e => CGraph n e -> UINode -> [UINode]
verticallyConnectedNodes g n =
  VU.toList $
    goUp (parentsVertical g n)
      VU.++ VU.cons n (goDown (childrenVertical g n))
  where
    goUp nodes
      | VU.null nodes = VU.empty
      | otherwise =
        nodes
          VU.++ VU.concatMap (goUp . parentsVertical g) nodes
    goDown nodes
      | VU.null nodes = VU.empty
      | otherwise =
        nodes
          VU.++ VU.concatMap (goDown . childrenVertical g) nodes

------------------------------------------------------------------------------------------------------
-- * Borders of cells
--   Cells have a nesting and border type, when a box has to be drawn around a graph

data LayerFeatures = LayerFeatures
  { layer :: Nesting, -- ^Graphs that are inside graphs get a higher nesting value (I use this to make every new layer a little bit darker). This is used to calculate the subgraph windows
    border :: Maybe Border -- ^Set the css values (border, boxshadow)
  }
  deriving (Show, Generic)

instance FromJSON LayerFeatures

instance ToJSON LayerFeatures

type Nesting = Int

lb :: Int -> Maybe LayerFeatures
lb n = Just (LayerFeatures n (Just LeftBorder))

rb :: Int -> Maybe LayerFeatures
rb n = Just (LayerFeatures n (Just RightBorder))

tb :: Int -> Maybe LayerFeatures
tb n = Just (LayerFeatures n (Just TopBorder))

bb :: Int -> Maybe LayerFeatures
bb n = Just (LayerFeatures n (Just BottomBorder))

ltb :: Int -> Maybe LayerFeatures
ltb n = Just (LayerFeatures n (Just LeftTopBorder))

rtb :: Int -> Maybe LayerFeatures
rtb n = Just (LayerFeatures n (Just RightTopBorder))

lbb :: Int -> Maybe LayerFeatures
lbb n = Just (LayerFeatures n (Just LeftBottomBorder))

rbb :: Int -> Maybe LayerFeatures
rbb n = Just (LayerFeatures n (Just RightBottomBorder))

mid :: Int -> Maybe LayerFeatures
mid n = Just (LayerFeatures n Nothing)

data Border
  = LeftBorder
  | RightBorder
  | TopBorder
  | BottomBorder
  | LeftTopBorder
  | RightTopBorder
  | LeftBottomBorder
  | RightBottomBorder
  deriving (Show, Generic)

instance FromJSON Border

instance ToJSON Border

------------------------------------------------------------------------------------------------------
-- * Helper functions
--

myFromJust :: Int -> Maybe a -> a
myFromJust i term
  | isJust term -- Debug.Trace.trace ("myFromJustTrue "++ show i)
    =
    fromJust term
  | otherwise =
    Debug.Trace.trace
      ("myFromJust " ++ show i)
      fromJust
      term

myHead :: Int -> [a] -> a
myHead i a
  | null a = error ("head: empty list " ++ show i)
  | otherwise = head a

myLast :: Int -> [a] -> a
myLast i a
  | null a = error ("last: empty list " ++ show i)
  | otherwise = last a

vHead :: (VU.Unbox a) => Int -> VU.Vector a -> a
vHead i a
  | VU.null a = error ("VU.head: empty list " ++ show i)
  | otherwise = VU.head a

rmdups :: (Ord a) => [a] -> [a]
rmdups = map (myHead 500) . group . sort
