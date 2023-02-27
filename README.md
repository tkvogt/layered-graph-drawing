# Layered Graph Drawing
Sugiyama-style graph drawing: https://en.wikipedia.org/wiki/Layered_graph_drawing

## Overview Paper:
[An Efficient Implementation of Sugiyama’s
Algorithm for Layered Graph Drawing](https://www.elibm.org/ft/10011396000)

## Sugyama consists of several steps:

1. A directed graph

![Initial graph](https://raw.githubusercontent.com/BeFunctional/layered-graph-drawing/main/graphs/t0.svg)

2. Find the longest path

![Longest path](https://raw.githubusercontent.com/BeFunctional/layered-graph-drawing/main/graphs/t1.svg)

3. Crossing Reduction

![Crossing reduction](https://raw.githubusercontent.com/BeFunctional/layered-graph-drawing/main/graphs/t2.svg)

Using: [Simple and Eﬃcient Bilayer Cross Counting](http://ls11-www.cs.tu-dortmund.de/downloads/papers/BJM04.pdf)

4. Y-placement (Not fully implemented)

Using: [Fast and Simple Horizontal Coordinate Assignment](https://arxiv.org/abs/2008.01252)
