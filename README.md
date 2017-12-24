<p align="center">
    <img src="https://kripken.github.io/talks/wasm3.png" width="400px"/>
</p>

Haskell WebAssembly
-------------------

[![Build Status](https://travis-ci.org/haskell-wasm/wasm.svg?branch=master)](https://travis-ci.org/haskell-wasm/wasm)

A WebAssembly AST, parser, and assembler in Haskell for use in functional compiler backends
targeting WebAssembly.

Warning, still a *big* work in progress.

#### Codebase

Core modules

* [Entry](https://github.com/sdiehl/wasm/blob/master/src/Language/Wasm/Entry.hs) - Driver
* [Syntax](https://github.com/sdiehl/wasm/blob/master/src/Language/Wasm/Syntax.hs) - Frontend AST
* [Parser](https://github.com/sdiehl/wasm/blob/master/src/Language/Wasm/Parser.y) - Parser
* [Lexer](https://github.com/sdiehl/wasm/blob/master/src/Language/Wasm/Lexer.x) - Lexer
* [Pretty](https://github.com/sdiehl/wasm/blob/master/src/Language/Wasm/Pretty.hs) - Textual Format
* [Binary](https://github.com/sdiehl/wasm/blob/master/src/Language/Wasm/Binary.hs) - Binary Format
* [Test](https://github.com/sdiehl/wasm/blob/master/src/Test.hs) - Test suite

#### Usage

TODO
