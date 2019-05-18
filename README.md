# Online BDD Simulation Tool (obst)

Binary Decision Diagrams (BDDs, see also [Wikipedia](https://en.wikipedia.org/wiki/Binary_decision_diagram)) are a compact way of representing sets of numbers, so that certain set operations (e.g. union, intersection, complement) can still be executed efficiently. They are used extensively in logic synthesis as well as formal verification.

obst is an application that shows how the algorithms for constructing and operating on BDDs work on a step-by-step basis. You can try it out [here](https://nicze.de/philipp/bdds). It is intended to be useful for both exploring the inner workings of BDDs interactively, as well as demonstrating the intermediate steps of a BDD computation in a classroom environment.

It has both a web interface as well as a native one, the latter of which currently runs only on Linux. While the functionality is almost identical, the native version is more performant and everything feels better.

As the web interface of obst is built on WebAssembly, it requires a reasonably modern browser to run. I have tested it on both Firefox 61 and Chrome 68. If you want to be able to run the website offline, either grab one of the releases from the GitHub repository, or just download `index.html`, `obst.js`, and `obst.wasm` from the live version.

obst was written by Philipp Czerner in 2018. You can contact me via e-mail (contact@nicze.de) or on GitHub. Do feel free to send me any feedback, suggestions, etc., I would love to hear them!

## Usage instructions

I hope the application is pretty self-explanatory, but a few details are not documented there. So, here is the full list of keybindings:

* *Left, Right*: Move to the previous/next frame of the animation.
* *Page down, Page up*: Move to the previous/next checkpoint, that is the first frame of an operation.
* *Home, End*: Move to the first/last frame.
* *F1*: Show/hide the help.
* *F2*: Show performance information. Draws last, max and average frame times for the last 128 frames, in 0.1ms. Note that if no animation is running, no frames will be drawn, including the debug information. Also note that the performance numbers are just measuring the delay inside obst and do not account for the browser overhead, so your actual framerate will be much lower than suggested. Finally, performance measurements on websites are precise to about 1ms, unless you disable the timing attack mitigation of your browser.
* *F3*: Show/Hide the left panel.
* *F11*: Toggle fullscreen.
* *Ctrl-+*, *Ctrl--*, *Ctrl-0*, Zoom in/out, or reset zoom.

The last three are implemented by me only in the native version, for the web interface this functionality is provided by the browser. They should be on the same keys, unless you rebound them.

When entering lists of numbers, I claim to accept 'comma-separated lists of numbers'. However, it is actually 'lists of alphanumerical characters separated by non-alphanumerical characters'. The digits are 0-9, a-z (case insensitive). Depending on your base, only some of these are valid. 

For entering nodes, just write down the name of the node. 

The 'Bit order' field determines the mapping between levels and bits. If you leave it at 'auto', the number of levels is chosen as the largest index of the highest bit of each number, and the order is such that higher levels correspond to more significant bits.

The parser for boolean formulae accepts a list of statements separated by either newlines or semicolons. Each statement is either of the form `x = <expr>` or `<expr>`, with `<expr>` being a boolean expression using negation `~` and the following binary operators: `&`, `|`, `->`, `<-`, `^`, `<->`. I accept a few variants of these operators, notably their usual unicode symbols ¬, ∧, ∨, ←, →, ⊕, ↔. (If you want the full list, see `_get_operator_type` in `obst.cpp`.) The operators are listed in order of precedence, `<-` is right-associative. You can use the constants `0` and `1` as well as arbitrary identifiers. `x = <expr>` assigns `x` to be the subformula `<expr>`. Unassigned identifiers are treated as variables. The statement '<expr>' adds the expression to the formula the bdd stores. Lines beginning with `#` are ignored.

Variable order is the string 'auto' or a comma-separated list of variable names. (Actually, a list of identifiers and operators, the latter of which are ignored.)

## Building and running the web interface

Take care to set up your Emscripten environment beforehand, e.g. by executing `source /path/to/emscripten/emsdk_env.sh`.

Debug build:

    ./build.sh emcc debug

Release build:

    ./build.sh emcc release

To give a bit more context, the project consists of a few C++ source files, as well as a small HTML page. You need to compile and link only `platform_emscripten.cpp`. C++14 has to be enabled. Apart from that, no special treatment is necessary. I used `emcc` version 1.38.12 during development.

Testing the web page locally can be done by simply pointing the browser at the `index.html` file. However, the code will complain about not being able to perform a WebAssembly streaming compile, as the MIME type is not set correctly. For this, run

    emrun index.html

and ignore the warning about stdout capture (the console in the browser will still work just fine). This hosts a local webserver, which advertises the correct MIME type.

## Building and running the native interface

A simple

    ./build.sh gcc debug

suffices for a debug build. This will compile and link `platform_linux.cpp`. You can do that by hand, if you prefer. In terms of dependencies, you need OpenGL 3.2, X11 and Xrandr (link `-lGL -lX11 -lXrandr`), which should be present on any Linux desktop set up in the last decade. C++14 support has to be enabled.

    ./build.sh gcc release

This creates the release build. There is one additional consideration for shipping: I prefer distributing just a single executable. Hence, you can run

    ./obst --pack

to pack the necessary ressources (the fonts and their license) into the binary. This will create an `obst_packed` executable. It still needs the dynamic libraries present at runtime, but I am reasonably confident that these are preinstalled on any somewhat modern system.

## The source code

Here, I will give a quick high level overview, so that you have an easier time locating the part you want to modify or understand. Asking me is, of course, also an option.

There are a lot of comments in the code and I have tried to explain all the interesting tricks in detail. So if you are curious about how a specific piece of functionality works, there is a good chance the code contains a page-long description of it.

The program is strucured into the following layers:

1. BDD algorithms. These execute the actual algorithms on a BDD data structure, and generate step-by-step snapshots of the current graph, together with context information explaining the current step in textual form. The most relevant functions all contain `stepwise` in their name (`bdd_from_list_stepwise`, `bdd_union_stepwise`, etc.). The whole layer uses the `bdd_` prefix.

2. Graph layout. In the previous layer we treat the graph simply as a graph. Now, we construct an embedding into 2D space, i.e. we compute positions for all nodes and edges. (I call this mapping of nodes and edges to positions a layout.) This works in an incremental fashion: Starting from some layout and taking a graph (i.e. the state of the BDD at a certain point in time) as input, we generate a layout that is 'close' to the one we started with. In particular, the relative positions of nodes and edges are preserved. (If node 15 was to the left of node 18, and both nodes still exist, then node 15 will continue to be left of node 18.) Most of the work is done in `layout_graph`, which uses a force-based simulation to generate aesthetically pleasing node positions, as well as a lot of bookkeeping to find an initial configuration. In an additional step, straight edges are replaced by quadratic splines, in `layout_splinify`.

3. Graph drawing via WebGL. At some point, we want to be able to draw elliptical nodes, quadratic splines and node labels. The necessary abstrations for this are provided by this layer, using a hideous amount of OpenGL boilerplate and some non-trivial fragment shaders.

4. Layout interpolation. Both the graph layout and the graph drawing layers deal with a single layout at a time. (Well, the former does it incrementally.) But for the actual animations we need to interpolate between two layouts, which is realised in the `layout_frame_draw` function. It does all the work computing the correct animations and issuing the right draw calls.

5. UI. Finally, someone has to react to button presses and glue the lower layers together. There is not a lot of interesting stuff happening here.

6. Platform. This provides an API for the UI to interact with the underlying platform and calls into the UI for initialisation and updates.

  1. Linux. Here the window is in the tender and loving hands of X, while we render using OpenGL. Fonts are loaded from disk and rasterised using `stb_truetype`. This layer contains an implementation of a GUI.

  2. Emscripten. The browser is responsible for everything, so most things emit the appropiate HTML code instead of doing any actual work. `index.html` defines the GUI structure, WebGL is used for rendering.

Some random implementation details:

* In the code I use the word 'BDD' to refer to nodes in a BDD.
* IDs 0 and 1 are reserved for the False and True nodes. These have both children pointing to themselves.
* The layout algorithm uses a clever trick to get the nodes to not overlap: As long as nodes are closer than the minimum distance, all forces except the one pushing them apart treat them as if they were glued together.
* Layouts are computed going backwards in time, so the iterative graph layout starts with the last frame.
* Both the ellipses and quadratic spline are drawn in the fragment shader.
* To have accurate gaps while drawing the quadratic splines, the relationship between t (the curve parameter) and the length of the curve, which is decidedly non-linear, is approximated by a third-degree polynomial for the fragment shader.
* Text is rendered by first getting the browser to draw it in a 2D canvas environment, and then copying it onto a WebGL texture. This process is performed whenever the scale changes.
* Labels are offset by a subpixel amount to make the glyph texture map exactly onto the screen pixels. But that is only for the start and end points of an animation, the others are interpolated normally so that the animation feels smooth.

## Trivia

* The active node (drawn in red) has a thicker border
* The background of coloured nodes is not white, but a very washed-out version of their colour
* Arrows do not end precisely where the border begins, but a tiny bit later
* Interpolation is not linear. Instead it is smoothed out a bit.
