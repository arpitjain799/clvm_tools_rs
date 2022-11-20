#!/bin/sh

cd fuzz/generate-types && \
cargo rustc -- \
		  -C 'passes=sancov-module' \
		  -C llvm-args='-sanitizer-coverage-level=3' \
		  -C llvm-args='-sanitizer-coverage-inline-8bit-counters' \
		  -Z sanitizer=address \
		  --cfg='feature="fuzzer"' && \
./target/debug/fuzz-types
