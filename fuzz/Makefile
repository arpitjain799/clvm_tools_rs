all:
	# sancov
	cargo rustc -- -C llvm-args='-sanitizer-coverage-level=3' -C llvm-args='-sanitizer-coverage-inline-8bit-counters' -Z sanitizer=address --cfg='feature="fuzzer"'
