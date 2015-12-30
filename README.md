<p align="center">
    <img src="https://kripken.github.io/talks/wasm3.png" width="400px"/>
</p>

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
* [Binary Encoding](https://github.com/WebAssembly/design/blob/master/BinaryEncoding.md)
* [ilwasm](https://github.com/WebAssembly/ilwasm)
* [binaryren](https://github.com/WebAssembly/binaryen)
* [wassembler](https://github.com/ncbray/wassembler)
* [demo](https://ncbray.github.io/wassembler/)
* [backend prototype](https://github.com/ncbray/wassembler/blob/master/v8/backend.js)

**Binary Protocol**

* [v8 wasm](https://github.com/v8/v8/tree/master/src/wasm)
* [wasm.h](https://github.com/WebAssembly/sexpr-wasm-prototype/blob/master/src/wasm.h)
* [wasm-binary-writer.c](https://github.com/WebAssembly/sexpr-wasm-prototype/blob/master/src/wasm-binary-writer.c)

#### Goals

* Parse WebAssembly textual AST
* Read binary format
* Emit binary format
* Pretty print textual AST
* Verify soundness of code
* Interpreter
* High level code generation API  for retargeting
* Translation to [LLVM IR backend](http://reviews.llvm.org/D10569)

### Build with `stack` (recommended)

```bash
$ git clone https://github.com/sdiehl/wasm
$ cd wasm
$ stack install
```

```bash
$ wasm test/fac.wast
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

#### Building v8

```bash
$ git clone git@github.com:v8/v8.git
$ make native wasm=on

$ out/native/v8 --expose-wasm
V8 version 4.9.0 (candidate)
d8> buffer = readbuffer('test.bin');
[object ArrayBuffer]
d8> module = WASM.instantiateModule(buffer, {});
{memory: [object ArrayBuffer], test: function test() { [native code] }}
d8> module.test()
3
```

#### Codebase

Core modules

* [Syntax](https://github.com/sdiehl/wasm/blob/master/src/Syntax.hs) - Frontend AST
* [Eval](https://github.com/sdiehl/wasm/blob/master/src/Eval.hs)  - Interpreter
* [Parser](https://github.com/sdiehl/wasm/blob/master/src/Parser.y) - Parser
* [Lexer](https://github.com/sdiehl/wasm/blob/master/src/Lexer.x) - Lexer
* [Pretty](https://github.com/sdiehl/wasm/blob/master/src/Pretty.hs) - Pretty Printer
* [Text](https://github.com/sdiehl/wasm/blob/master/src/Test.hs) - Test suite

#### Example

```scheme
(module

  ;; Recursive factorial
  (func (param i64) (result i64)
    (if_else (i64.eq (get_local 0) (i64.const 0))
      (i64.const 1)
      (i64.mul (get_local 0) (call 0 (i64.sub (get_local 0) (i64.const 1))))
    )
  )

)
```


```scheme
(module
  (export "test" 0)
  (func (result i32)
    (i32.add (i32.const 1) (i32.const 2))))
```


Will generate

```
0000000 0101 0100 0102 0009 1500 0000 0500 4000
0000010 0109 0209 7406 7365 0074               
000001a
```
