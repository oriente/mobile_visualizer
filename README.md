mobile_visualizer
=================
This project implements Html5 visualization of Gauss-Markov mobility trace file from Network Simulator 3 (ns-3) using Blank-Canvas library in Haskell.

It has 3 main modules.
1) PowerBar.hs implements the color gradient display from green to red, which is used to graphically represent the quality of network measure

2) GraphConstruction.hs includes the implementation of setting up node and link coordinates. It also compute graph theoretical properties flow robustness for a specific network. Data.Graph Library is used to construct a graph from a list node position information.

3)nodeVisualizer.hs reads information from trace file and render all components into Canvas. Three keyboard events are enabled to control the action of visualization. 'F' tells the time step moving forward. 'B' tells the time step moving backward. 'S' tells the time step to stop.

=================
how to run:
$ ghc nodeVisualizer.hs
./nodeVisualizer 1989 < w20n4s1001v10.mob

the first argument 1989 is the port number of http server so that you can run in multiple web pages with different port number simultaneously. w20n4s1001v10.mob is the sample mobility trace file used for testing.

=================
implementation notes:


