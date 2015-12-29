Haskell WebAssembly
-------------------

A portable machine architecture for the web.

> Define a portable, size- and load-time-efficient binary format to serve as a
> compilation target which can be compiled to execute at native speed by taking
> advantage of common hardware capabilities available on a wide range of
> platforms, including mobile and IoT.

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
