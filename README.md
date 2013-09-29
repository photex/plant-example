plant-example
=============

An example of a project that uses plant.


An outline of the steps taken to setup the plant project:
- mkdir plant-example
- cd plant-example
- plant setup cl-opengl cl-glut sb-cga cl-graph
- plant include git git://github.com/photex/lofi-tri.git
- plant quickloads lofi.tri

At this point you can just `plant swank` and connect with slime to interact with this project.

What does this example actually do?
===================================

From slime or the repl:
- (ql:quickload :plant-example)
- (plant-example:demo 100)

You should see an opengl window with a random 2d delaunay triangulation.
