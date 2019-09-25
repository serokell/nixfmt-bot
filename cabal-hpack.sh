#!/bin/sh
# save this file as cabal-hpack and run instead of cabal 
find . -maxdepth 6 -name package.yaml -exec hpack {} \;
exec cabal "$@"
