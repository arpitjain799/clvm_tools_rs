all:
	cargo rustc -- \
		-C 'passes=sancov-module' \
		-C llvm-args='-sanitizer-coverage-level=3' \
		-C llvm-args='-sanitizer-coverage-inline-8bit-counters' \
		-Z sanitizer=address \
		--cfg='feature="fuzzer"'

run: all
		./target/debug/fuzz-types
