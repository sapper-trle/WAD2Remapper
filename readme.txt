WAD2Remapper 1.0 readme.txt

WARNING: Use this program only on a copy of your wad2. The wad2 file format is
easily corrupted.

For an outfit to join properly in game, the vertices (points) that connect to
a joint mesh must have an index equal or less than a certain number (limit).

Vertices that meet this requirement are shown green. Vertices that are above the
limit are shown red.

So you have to swap any red vertices that connect to a joint mesh with a green
vertex.

There are two ways to swap vertices.

1) Left click a vertex to select it and then hold "Control" and left click on
   another vertex to swap them.

2) Enter the two vertex numbers to swap in the boxes and click "Swap".

When a vertex is selected in the 3D viewport the vertex will be displayed larger
and its index will be shown beneath the 3D viewport and also in the "V1" box.

When an index is selected in the "V1" box it will be selected in the 3D viewport.

When a mesh is selected in the treeview the number of vertices in the mesh will
be displayed and the limit index number for the mesh will be displayed.

When a vertex is selected it's index number and coordinates will be displayed.

In the treeview the moveables in the wad2 are displayed with their slot number.
So Moveable8 is LARA_SKIN.

The pattern of vertex index numbers must be the same for all hand meshes and the
same for all head meshes.

The holes for the ponytail/braid must have vertex indices as shown in the images.

3D Viewport Controls.

Left mouse button - Select vertex
Right mouse button - Rotate view
Middle mouse button - Pan view
Mouse wheel - Zoom view

Note that sometimes you might click through a mesh and select the vertex hidden
behind the mesh.

Usage:
 Open your wad2.
 Expand a moveable in the treeview.
 Select a mesh.
 Swap vertices as required.
 Save the wad2.

A Firemonkey (FMX) 3D application built using free Delphi Community Edition 10.3.3

sapper
June 2021
