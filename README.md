# Online BDD Simulation Tool (obst)

Binary Decision Diagrams (BDDs, see also [Wikipedia](https://en.wikipedia.org/wiki/Binary_decision_diagram)) are a compact way of representing sets of numbers, so that certain set operations (e.g. union, intersection, complement) can still be executed efficiently. They are used extensively in logic synthesis as well as formal verification.

obst is a web application that shows how the algorithms for constructing and operating on BDDs work on a step-by-step basis. You can try it out [here](https://nicze.de/philipp/bdds). It is intended to be useful for both exploring the inner workings of BDDs interactively, as well as demonstrating the intermediate steps of a BDD computation in a classroom environment.

As obst is built on WebAssembly, it requires a reasonably modern browser to run. I have tested it on both Firefox 61 and Chrome 68. If you want to be able to run the website offline, either grab one of the releases from the GitHub repository, or just download `index.html`, `obst.js`, and `obst.wasm` from the live version. Currently, obst only runs in a website. If you are interested in running it as a native application over a CLI, e.g. to generated a pdf slideshow out of the animation, feel free to contact me.

obst was written by Philipp Czerner in 2018. You can contact me via e-mail (contact@nicze.de) or on GitHub. Do feel free to send me any feedback, suggestions, etc., I would love to hear them!

## Usage instructions

I hope the website is pretty self-explanatory, but a few keyboard shortcuts are not documented there. So, here is the full list of keybindings:

* *Left, Right*: Move to the previous/next frame of the animation.
* *Page down, Page up*: Move to the previous/next checkpoint, that is the first frame of an operation.
* *Home, End*: Move to the first/last frame.
* *Shift-1* (well, `!` actually): Show performance information. Draws last, max and average frame times for the last 128 frames, in 0.1ms. Note that if no animation is running, no frames will be drawn, including the debug information. Also note that the performance numbers are just measuring the delay inside obst and do not account for the browser overhead, so your actual framerate will be much lower than suggested. Finally, performance measurements on websites are precise to about 1ms, unless you disable the timing attack mitigation of your browser.

When entering lists of numbers, I claim to accept 'comma-separated lists of numbers'. However, it is actually 'lists of alphanumerical characters separated by non-alphanumerical characters'. The digits are 0-9, a-z (case insensitive). Depending on your base, only some of these are valid.

For entering nodes, just write down the index of the node. `T` and `F` also work, respectively denoting the True and False node.

The 'Bit order' field determines the mapping between levels and bits. If you leave it at 'auto', the number of levels is chosen as the largest index of the highest bit of each number, and the order is such that higher levels correspond to more significant bits.

## Building and running the program

Take care to set up your Emscripten environment beforehand, e.g. by executing `source /path/to/emscripten/emsdk_env.sh`.

Debug build:

    ./build.sh

Release build:

    ./build_release.sh

To give a bit more context, the project consists of a single C++ source file, as well as a small HTML page. You need to define the OS_EMSCRIPTEN macro while compiling, as well as enabling C++14. Apart from that, no special treatment is necessary. I used `emcc` version 1.38.12 during development.

Testing the web page locally can be done by simply pointing the browser at the `index.html` file. However, the code will complain about not being able to perform a WebAssembly streaming compile, as the MIME type is not set correctly. For this, run

    emrun index.html

and ignore the warning about stdout capture (the console in the browser will still work just fine). This hosts a local webserver, which advertises the correct MIME type.

## The source code

Here, I will give a quick high level overview, so that you have an easier time locating the part you want to modify or understand. Asking me is, of course, also an option.

There are a lot of comments in the code and I have tried to explain all the interesting tricks in detail. So if you are curious about how a specific piece of functionality works, there is a good chance the code contains a page-long description of it.

The program is strucured into the following layers:

1. BDD algorithms. These execute the actual algorithms on a BDD data structure, and generate step-by-step snapshots of the current graph, together with context information explaining the current step in textual form. The most relevant functions all contain `stepwise` in their name (`bdd_from_list_stepwise`, `bdd_union_stepwise`, etc.). The whole layer uses the `bdd_` prefix.

2. Graph layout. In the previous layer we treat the graph simply as a graph. Now, we construct an embedding into 2D space, i.e. we compute positions for all nodes and edges. (I call this mapping of nodes and edges to positions a layout.) This works in an incremental fashion: Starting from some layout and taking a graph (i.e. the state of the BDD at a certain point in time) as input, we generate a layout that is 'close' to the one we started with. In particular, the relative positions of nodes and edges are preserved. (If node 15 was to the left of node 18, and both nodes still exist, then node 15 will continue to be left of node 18.) Most of the work is done in `layout_graph`, which uses a force-based simulation to generate aesthetically pleasing node positions, as well as a lot of bookkeeping to find an initial configuration. In an additional step, straight edges are replaced by quadratic splines, in `layout_splinify`.

3. Graph drawing via WebGL. At some point, we want to be able to draw elliptical nodes, quadratic splines and node labels. The necessary abstrations for this are provided by this layer, using a hideous amount of OpenGL boilerplate and some non-trivial fragment shaders.

4. Layout interpolation. Both the graph layout and the graph drawing layers deal with a single layout at a time. (Well, the former does it incrementally.) But for the actual animations we need to interpolate between two layouts, which is realised in the `layout_frame_draw` function. It does all the work computing the correct animations and issuing the right draw calls.

5. UI. Finally, someone has to react to button presses and glue the lower layers together. There is not a lot of interesting stuff happening here.

Some random implementation details:

* In the code I use the word 'BDD' to refer to nodes in a BDD.
* IDs 0 and 1 are reserved for the False and True nodes. These have both children pointing to themselves.
* The layout algorithm uses a clever trick to get the nodes to not overlap: As long as nodes are closer than the minimum distance, all forces except the one pushing them apart treat them as if they were glued together.
* Layouts are computed going backwards in time, so the iterative graph layout starts with the last frame.
* Both the ellipses and quadratic spline are drawn in the fragment shader.
* To have accurate gaps while drawing the quadratic splines, the relationship between t (the curve parameter) and the length of the curve, which is decidedly non-linear, is approximated by a third-degree polynomial for the fragment shader.
* Text is rendered by first getting the browser to draw it in a 2D canvas environment, and then copying it onto a WebGL texture. This process is performed whenever the scale changes, in the hope of having pixel-perfect text.

## Trivia

* The active node (drawn in red) has a thicker border
* The background of coloured nodes is not white, but a very washed-out version of their colour
* Arrows do not end precisely where the border begins, but a tiny bit later
* Interpolation is not linear. Instead it is smoothed out a bit.
* Labels are offset by a subpixel amount to make the glyph texture map exactly onto the screen pixels. But that is only for the start and end points of an animation, the others are interpolated normally so that the animation feels smooth.
