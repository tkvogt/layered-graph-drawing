# Layered Graph Drawing
Sugiyama-style graph drawing: https://en.wikipedia.org/wiki/Layered_graph_drawing

## Overview Paper:
[An Efficient Implementation of Sugiyama’s
Algorithm for Layered Graph Drawing](https://www.elibm.org/ft/10011396000)

## Sugyama consists of several steps:

Starting with

1. A directed graph

![Initial graph](https://raw.githubusercontent.com/BeFunctional/layered-graph-drawing/main/graphs/g0.svg)

2. Find the longest path and add connection vertices

Find the nodes that have no children and walk backwards, putting all parent nodes in layers.
If a connection line passes over several layers, a node has to be added in every layer.

![Longest path](https://raw.githubusercontent.com/BeFunctional/layered-graph-drawing/main/graphs/g1.svg)

3. Crossing Reduction

![Crossing reduction](https://raw.githubusercontent.com/BeFunctional/layered-graph-drawing/main/graphs/g2.svg)

Using: [Simple and Eﬃcient Bilayer Cross Counting](http://ls11-www.cs.tu-dortmund.de/downloads/papers/BJM04.pdf)

4. Y-placement (works, but is not an average of alignments)

![y placement](https://raw.githubusercontent.com/BeFunctional/layered-graph-drawing/main/graphs/g3.svg)

Using: [Fast and Simple Horizontal Coordinate Assignment](https://arxiv.org/abs/2008.01252)

## Algorithm extensions (vertical and virtual edges)

Sometimes nodes have to to drawn vertically below each other. To force the algorithm to do this, you have to connect them with vertical edges. The edges are not drawn, but guide the algorithm, which unfortunelty made the algorithms (especially longest path) more complex.

Virtual edges are also not drawn but are used when several unconnected graphs have to be displayed in a a row. I don't know if this was the best decision.

## Putting the graph in a box

The module Graph/SubgraphWindows contains an algorithm that calculates boxes around graphs and subgraphs by using a nesting attribute of the node, adding border and nesting attributes to every cell of the html table.

This algorithm was implemented to develop a visual programming environment, which is work in progress: www.autocompletion.io

## TODO
 * Make graphs inside graphs look better by embedding a layouted graph inside a graph.

