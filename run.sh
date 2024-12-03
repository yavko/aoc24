#!/bin/bash
ghc -dynamic Main.hs -outputdir ./build && ./Main "$@"