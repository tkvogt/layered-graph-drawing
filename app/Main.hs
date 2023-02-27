{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Word (Word32)
import Graph.AjaxStructures (CGraph, EdgeType (..), FunctionNode (..), PathInfo (..), SpecialNode (..), UIEdge (..), UINodeLabel (..))
import Graph.GraphDrawing

testGraph :: CGraph
testGraph =
  fromAdj
    numbers
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

testGraph2 :: CGraph
testGraph2 =
  fromAdj
    numbers
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
optionsGraph :: CGraph
optionsGraph =
  fromAdj
    numbers
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

numbers :: Map.Map Word32 UINodeLabel
numbers = Map.fromList (map (\x -> (x, ulabel x)) [1 .. 100])
  where
    ulabel n =
      UINodeLabel
        ( FuN
            ( FunctionNode
                ""
                (T.pack (show n))
                ""
                ""
                ""
                False
                False
                False
                (Nothing, Nothing)
                1
                JustConnected
                False
                0
            )
        )
        Nothing
        Nothing

dEdge :: [UIEdge]
dEdge = [UIEdge "standardEdge" Nothing Nothing Nothing Nothing Nothing "" Nothing 0 NormalEdge]

-- [UIEdge 2 1 "" Curly "#ff5863" "" 0 False False]

vdummyEdge :: [UIEdge]
vdummyEdge = [UIEdge "standardEdge" Nothing Nothing Nothing Nothing Nothing "" Nothing 0 VirtualHorEdge]

-- [UIEdge 2 1 "" Curly "#ff5863" "" 0 True False]

main :: IO ()
main = putStrLn ""

-- mainWith (visualGraph (layeredGraph testGraph) :: Diagram B)

------------------------------------------------------------------------------------------------
{-
-- Using diagrams to visualise the graph (this is commented out, so that you only need the quite big diagrams dependency when using this for debugging)

visualGraph :: CGraph -> Diagram B
visualGraph g = Debug.Trace.trace (show (length shortEs) ++ " : " ++ show (length longEs)) $
                position graphNodes # connections shortEs
                                    # connectionsWithoutArrow longEs
  where graphNodes = map pos (zip (map fromIntegral (Map.keys nLabels)) (Map.elems nLabels))
        nLabels = Graph.nodeLabels g
        pos (n, (UINodeLabel x y _ _ _ _ _)) = (p2 ((fromIntegral x)*(2), (fromIntegral y)), vNode n)
        es = edges g
        shortEs = filter (\(Graph.DirEdge src tgt) -> not (isConnNode g src) && not (isConnNode g tgt)) es
        longEs  = filter (\(Graph.DirEdge src tgt) ->     (isConnNode g src) ||     (isConnNode g tgt)) es
        vNode :: Int -> Diagram B
        vNode n = visualNode (isDummyNode g (fromIntegral n)) (show n)

-----------------------------------------------------------------------------------------------

connections es | (length es) < 2 = id
               | otherwise = foldr1 (.) $
                             map (\(Graph.DirEdge src tgt) ->
                                   connectOutside' arrowStyle2 (show src) (show tgt) ) es

connectionsWithoutArrow es | (length es) < 2 = id
                           | otherwise = foldr1 (.) $
                                         map (\(Graph.DirEdge src tgt) ->
                                                connectOutside' arrowStyle3 (show src) (show tgt) ) es

visualNode l str | l         = (stateLabel str <> dummy) # named str
                 | otherwise = (stateLabel str <> state) # named str

text' d s = (strokeP $ textSVG' (TextOpts (unsafePerformIO lin2) INSIDE_H KERN False d d) s) # lw none # fc black

stateLabel = text' 0.6
arrowLabel txt size = text' size txt

state = circle 0.4 # fc silver
dummy = circle 0.4 # fc red -- mempty

shaft  = arc xDir (-1/6 @@ turn)
shaft' = arc xDir (-2.7/5 @@ turn)
line = trailFromOffsets [unitX]

arrowStyle1 = (with  & arrowHead  .~ spike & headLength .~ small
                     & arrowShaft .~ shaft)

arrowStyle2 = (with  & arrowHead  .~ spike  & headLength .~ small
                     & arrowShaft .~ line)

arrowStyle3 = (with  & arrowHead  .~ noHead & arrowShaft .~ line & shaftStyle %~ dashingG [0.1, 0.05] 0)
-}
