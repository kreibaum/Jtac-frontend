# A Frontend for Jtac.jl

This is a little web frontend that is used to inspect models running with Jtac.
The game logic itself is implemented in the Jtac server, the frontend only
renders data it gets from a REST api.

https://github.com/roSievers/Jtac.jl/

Make sure that you have a running `julia server/aiserver.jl` and then run

    elm-live src/Main.elm -y http://localhost:4242 -x /api
