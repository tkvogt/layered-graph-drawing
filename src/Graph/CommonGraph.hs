{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Graph.CommonGraph where

import Data.List (group, sort)
import qualified Data.Vector.Unboxed as VU
import Data.Word (Word32)
import Graph.AjaxStructures
import qualified Graph.AjaxStructures as Ajax
import Graph.IntMap (Edge8 (..), Graph (..), adjacentNodesByAttr)
import qualified Graph.IntMap as Graph

rmdups :: (Ord a) => [a] -> [a]
rmdups = map (myhead 500) . group . sort

isDummy :: CGraph -> Node -> Bool
isDummy gr n = maybe False Ajax.isDummyLabel (Graph.lookupNode n gr)

isConnection :: CGraph -> Node -> Bool
isConnection gr n = maybe False Ajax.isConnLabel (Graph.lookupNode n gr)

isCase :: CGraph -> Node -> Bool
isCase gr n = maybe False Ajax.isCaseLabel (Graph.lookupNode n gr)

isFunction :: CGraph -> Node -> Bool
isFunction gr n = maybe False Ajax.isFuncLabel (Graph.lookupNode n gr)

isArgument :: CGraph -> Node -> Bool
isArgument gr n = maybe False Ajax.isArgLabel (Graph.lookupNode n gr)

isLiteral :: CGraph -> Node -> Bool
isLiteral gr n = maybe False Ajax.isLitLabel (Graph.lookupNode n gr)

someOption :: Option -> Bool
someOption ExpansionOption = True
someOption ExpansionStartType = True
someOption OnlyOption = True
someOption _ = False

childrenSeparating :: CGraph -> Word32 -> VU.Vector Word32
childrenSeparating gr n = adjacentNodesByAttr gr True n (Edge8 sepBit)

childrenNoVertical :: Graph UINodeLabel [UIEdge] -> Word32 -> VU.Vector Word32
childrenNoVertical gr n =
  (adjacentNodesByAttr gr True n (Edge8 virtBit))
    VU.++ (adjacentNodesByAttr gr True n (Edge8 0))

childrenVertical :: Graph UINodeLabel [UIEdge] -> Word32 -> VU.Vector Word32
childrenVertical gr n = adjacentNodesByAttr gr True n (Edge8 vertBit)

parentsVertical :: Graph UINodeLabel [UIEdge] -> Word32 -> VU.Vector Word32
parentsVertical gr n = adjacentNodesByAttr gr False n (Edge8 vertBit)

parentsNoVertical :: Graph UINodeLabel [UIEdge] -> Word32 -> VU.Vector Word32
parentsNoVertical gr n =
  (adjacentNodesByAttr gr False n (Edge8 virtBit))
    VU.++ (adjacentNodesByAttr gr False n (Edge8 0))

verticallyConnectedNodes :: CGraph -> UINode -> [UINode]
verticallyConnectedNodes g n =
  -- Debug.Trace.trace "verticallyConnectedNodes" $
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
