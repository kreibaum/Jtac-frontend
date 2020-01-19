# A Frontend for Jtac.jl

This is a little web frontend that is used to inspect models running with Jtac.
The game logic itself is implemented in the Jtac server, the frontend only
renders data it gets from a REST api.

To compile, run 

    elm-live src/Main.elm --no-server

and then symlink the index.html into a position where the Jtac server finds it.