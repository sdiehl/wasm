Haskell WebAssembly
-------------------

A portable machine architecture for the web.

> Define a portable, size- and load-time-efficient binary format to serve as a
> compilation target which can be compiled to execute at native speed by taking
> advantage of common hardware capabilities available on a wide range of
> platforms, including mobile and IoT.

* [Design](https://github.com/WebAssembly/design)
* [Semantics](https://github.com/WebAssembly/design/blob/master/AstSemantics.md)
* [Reference Implementation](https://github.com/WebAssembly/spec/tree/64822f7137e26c0b101ecba9cb1cd93d416c2c74/ml-proto)

#### Goals

* Parse WebAssembly textual AST
* Read binary format
* Emit binary format
* Pretty print textual AST
* Verify soundness of code

### Build with `stack` (recommended)

```bash
$ git clone https://github.com/sdiehl/wasm
$ cd wasm
$ stack install
```

```bash
$ wasm test/
```

### Build with `cabal`

```bash
$ git clone https://github.com/sdiehl/wasm
$ cd wasm

$ cabal sandbox init
$ cabal install
```

### How to Run Test Suite

```bash
$ stack test
```

### How to Run inside GHCi


```bash
$ stack ghci wasm
```
