{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Graph.CommonGraph where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.List (group, sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, isJust)
import qualified Data.Vector.Unboxed as VU
import Data.Word (Word32, Word8)
import Debug.Trace
import GHC.Generics
import Graph.IntMap
  ( Edge8 (..),
    EdgeAttribute (..),
    ExtractNodeType (..),
    Graph (..),
    adjacentNodesByAttr,
  )

type UINode = Word32

-- A Graph consists of nodes and edges, graph drawing arranges it from left to right,
-- start to end.
type CGraph n e = Graph (UINodeLabel n) [UIEdge e]

type CGraphL n e = (Graph (UINodeLabel n) [UIEdge e], Map UINode (Int, Int))

data UINodeLabel a = UINodeLabel
  { uinode :: a,
    nestingFeatures :: Maybe LayerFeatures,
    verticalNumber :: Maybe Word32 -- we want to keep the order of vertically connected Nodes,
    -- so we enumerate them during construction and examine them during graph drawing, when reducing crossings
  }
  deriving (Show, Generic)

type Channel = Int -- The nth type of a type node
-- This is path of function and type nodes with spaces that can be filled with options

data UIEdge a = UIEdge
  { edgeLabel :: a,
    channelNrIn :: Maybe Channel, -- arg nr x of constructor node

    -- | Me, Colleague-Name to remeber past decisions, but only people you know,
    -- or company, group
    channelNrOut :: Channel, -- function output is connected with input nr x of type node
    edgeType :: EdgeType
  }
  deriving (Show, Generic, Eq, Ord)

type GraphMoveX = Int

type Column = (GraphMoveX, [UINode])

instance Eq a => Eq (UINodeLabel a) where
  node1 == node2 = uinode node1 == uinode node2

class NodeClass a where
  isDummy :: StandardEdge e => CGraph a e -> UINode -> Bool
  isConnNode :: StandardEdge e => CGraph a e -> UINode -> Bool
  isFunction :: StandardEdge e => CGraph a e -> UINode -> Bool
  isMainArg :: CGraph a e -> UINode -> Bool
  isSubLabel :: UINodeLabel a -> Bool
  isArgLabel :: a -> Bool
  subLabels :: a -> Int
  connectionNode :: a
  dummyNode :: a

class StandardEdge e where
  standard :: e

------------------------------------------------------------------------------------------------------

dummyEdge :: StandardEdge e => UIEdge e
dummyEdge = UIEdge standard Nothing 0 NormalEdge

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

myhead :: Int -> [a] -> a
myhead i a
  | null a = error ("head: empty list " ++ show i)
  | otherwise = head a

vhead :: (VU.Unbox a) => Int -> VU.Vector a -> a
vhead i a
  | VU.null a = error ("VU.head: empty list " ++ show i)
  | otherwise = VU.head a

rmdups :: (Ord a) => [a] -> [a]
rmdups = map (myhead 500) . group . sort

------------------------------------------------------------------------------------------------------

vertBit :: Word8
vertBit = 0x1

virtBit :: Word8
virtBit = 0x2

sepBit :: Word8
sepBit = 0x4

instance StandardEdge e => EdgeAttribute [UIEdge e] where -- Why can two nodes be connected with more than one edge?
-- To connect one function with several input types that are part of one type node
  fastEdgeAttr (e : _) = f e
    where
      f (UIEdge _ _ _ VerticalEdge) = vertBit
      f (UIEdge _ _ _ VirtualHorEdge) = virtBit
      f (UIEdge _ _ _ _) = 0
  fastEdgeAttr _ = 0
  edgeFromAttr =
    Map.fromList
      [ (vertBit, [UIEdge standard Nothing 0 VerticalEdge]),
        (virtBit, [UIEdge standard Nothing 0 VirtualHorEdge]),
        (0, [UIEdge standard Nothing 0 NormalEdge])
      ]
  show_e (Just [UIEdge standard Nothing 0 e]) = show e
  show_e _ = "no Edge"

  bases _ = [Edge8 0, Edge8 vertBit, Edge8 virtBit]

childrenSeparating :: StandardEdge e => CGraph a e -> Word32 -> VU.Vector Word32
childrenSeparating gr n = adjacentNodesByAttr gr True n (Edge8 sepBit)

childrenNoVertical :: StandardEdge e => Graph (UINodeLabel a) [UIEdge e] -> Word32 -> VU.Vector Word32
childrenNoVertical gr n =
  (adjacentNodesByAttr gr True n (Edge8 virtBit))
    VU.++ (adjacentNodesByAttr gr True n (Edge8 0))

childrenVertical :: StandardEdge e => Graph (UINodeLabel a) [UIEdge e] -> Word32 -> VU.Vector Word32
childrenVertical gr n = adjacentNodesByAttr gr True n (Edge8 vertBit)

parentsVertical :: StandardEdge e => Graph (UINodeLabel a) [UIEdge e] -> Word32 -> VU.Vector Word32
parentsVertical gr n = adjacentNodesByAttr gr False n (Edge8 vertBit)

parentsNoVertical :: StandardEdge e => Graph (UINodeLabel a) [UIEdge e] -> Word32 -> VU.Vector Word32
parentsNoVertical gr n =
  (adjacentNodesByAttr gr False n (Edge8 virtBit))
    VU.++ (adjacentNodesByAttr gr False n (Edge8 0))

verticallyConnectedNodes :: StandardEdge e => CGraph a e -> UINode -> [UINode]
verticallyConnectedNodes g n =
  VU.toList $
    (goUp (parentsVertical g n))
      VU.++ (VU.cons n (goDown (childrenVertical g n)))
  where
    goUp nodes
      | VU.null nodes = VU.empty
      | otherwise =
        nodes
          VU.++ (VU.concatMap (\x -> goUp (parentsVertical g x)) nodes)
    goDown nodes
      | VU.null nodes = VU.empty
      | otherwise =
        nodes
          VU.++ (VU.concatMap (\x -> goDown (childrenVertical g x)) nodes)

------------------------------------------------------------------------------------------------------

data LayerFeatures = LayerFeatures
  { layer :: Int, -- Expanding a fuction generates a new layer
  -- (maybe make every new layer a little bit darker)
    border :: Maybe Border -- To set the right css values (border, boxshadow)
  }
  deriving (Show, Generic)

instance FromJSON LayerFeatures

instance ToJSON LayerFeatures

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

data EdgeType
  = NormalEdge
  | VerticalEdge -- When having options, they appear continuously in one column
  -- We mark this in the graph with vertical edges from the first
  -- option to the second and so on
  | VirtualHorEdge -- virtual edges are not displayed but used for layouting and
  -- naviagtion with the keyboard
  | SeparatingEdge -- to connect graph components that are separate
  deriving (Show, Generic, Eq, Ord)
