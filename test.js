buffer = readbuffer('example1.bin');
module = WASM.instantiateModule(buffer, {});
module.test();
