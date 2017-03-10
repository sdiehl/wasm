#!/bin/bash

set -e
set -x

step="$1"

echo "Running step $step ..."

case "$step" in
    install)
        mkdir -p $HOME/.local/bin
        case "$BUILD" in
            stack)
                stack $STACK_ARGS setup --no-terminal
                stack $STACK_ARGS build --fast --only-snapshot --no-terminal
                ;;
            cabal)
                sed -i 's/^jobs:/-- jobs:/' ${HOME}/.cabal/config;
                cabal new-build --enable-tests -j4 --dep wasm 
                ;;
        esac
        ;;
    script)
        case "$BUILD" in
            stack)
                stack $STACK_ARGS $STACK_BUILD_MODE --fast --no-terminal --skip-ghc-check $STACK_BUILD_ARGS
                ;;
            cabal)
                cabal new-build --enable-tests -j4 --dep wasm
                ;;
        esac
        ;;
    *)
        echo "Bad step: $step"
        exit 1
        ;;
esac

echo "Completed $step ."
exit 0
