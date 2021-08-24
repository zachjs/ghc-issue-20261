#!/usr/bin/env bash

set -e

try_with() {
    ghcup set ghc $1
    stack clean
    stack build --system-ghc --resolver "ghc-$1" 2>&1 \
        | grep "^!!! Renamer"
}

try_with 8.10.4
try_with 9.0.1
try_with 9.2.0.20210821
