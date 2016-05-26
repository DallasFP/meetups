This project contains an example in src/E01_Bare.hs which is written without the
use of transformers.

src/E02_Transformers.hs contains examples that just use the transformers package.

src/E03_Mtl.hs contains an example that uses the mtl package.

For practice, try implementing the example in src/E01_Bare.hs using the
transformers package and then the mtl package.

# Build instructions

Get [Stack](http://docs.haskellstack.org/en/stable/README/) and then all you
have to do is:

`stack build`

To watch files and continuously build:

`stack build --file-watch`

# Running instructions

To run the repl:

`env APP_SQLITE_FILE=derp.db APP_HTTP_PORT=5000 stack ghci`

To run the examples:

Bare: 

`env APP_SQLITE_FILE=derp.db APP_HTTP_PORT=5000 stack exec bare`

Transformers: 

`env APP_SQLITE_FILE=derp.db APP_HTTP_PORT=5000 stack exec transformers`

Mtl: 

`env APP_SQLITE_FILE=derp.db APP_HTTP_PORT=5000 stack exec mtl`
