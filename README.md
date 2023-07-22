# Layered Graph Drawing
Sugiyama-style graph drawing: https://en.wikipedia.org/wiki/Layered_graph_drawing

## Overview Paper:
[An Efficient Implementation of Sugiyama’s
Algorithm for Layered Graph Drawing](https://www.elibm.org/ft/10011396000)

## Sugyama consists of several steps:

Starting with

1. A directed graph without cycles

![Initial graph](https://raw.githubusercontent.com/BeFunctional/layered-graph-drawing/main/graphs/g0.svg)

2. Find the longest path and add connection vertices

Find the nodes that have no children and walk backwards, putting all parent nodes in layers.
If a connection line passes over several layers, a node has to be added in every layer.

![Longest path](https://raw.githubusercontent.com/BeFunctional/layered-graph-drawing/main/graphs/g1.svg)

3. Crossing Reduction
Using: [Simple and Eﬃcient Bilayer Cross Counting](http://ls11-www.cs.tu-dortmund.de/downloads/papers/BJM04.pdf)

![Crossing reduction](https://raw.githubusercontent.com/BeFunctional/layered-graph-drawing/main/graphs/g2.svg)

4. Y-placement (works, but is not an average of alignments)
Using: [Fast and Simple Horizontal Coordinate Assignment](https://kops.uni-konstanz.de/server/api/core/bitstreams/e3f1cd1e-3fd4-422d-852e-77404160f664/content)
![y placement](https://raw.githubusercontent.com/BeFunctional/layered-graph-drawing/main/graphs/g3.svg)

## Algorithm extensions (vertical and virtual edges, sub fields)

* Sometimes nodes have to to drawn vertically below each other. To force the algorithm to do this, you have to connect them with vertical edges. The edges are not drawn, but guide the algorithm, which unfortunelty made the algorithms (especially longest path) more complex.

* Virtual edges are also not drawn but are used when several unconnected graphs have to be displayed in a a row. I don't know if this was the best decision.

* A node can have sub fields, which is important when these sub fields are connected separately to other nodes. The number of this subfield (channel) is used in  Crossing reduction.

## Putting the graph in a box

The module Graph/SubgraphWindows contains an algorithm that calculates boxes around graphs and subgraphs by using a nesting attribute of the node, adding border and nesting attributes to every cell of the html table.

This algorithm was implemented to develop a visual programming environment, which is work in progress: www.autocompletion.io

## TODO
 * Make graphs inside graphs look better by embedding a layouted graph inside a graph.

## Example Usage
You can test this library by 
 * Uncommenting the ```diagrams```-code in [app/Main.hs](https://github.com/BeFunctional/layered-graph-drawing/blob/main/app/Main.hs) and uncommenting the ```diagrams```-dependencies in [layerd-graph-drawing.cabal](https://github.com/BeFunctional/layered-graph-drawing/blob/main/layered-graph-drawing.cabal)
 * ```cabal build``` or ```stack build```
 * generate the upper image by executing ```graph-drawing-exe -w 400 -o g3.svg```
