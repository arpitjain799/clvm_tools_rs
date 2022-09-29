all:
	cargo build --release
	cargo build --target --release wasm32-unknown-unknown && \
	wasm-bindgen --web --out-dir ./pkg ./target/wasm32-unknown-unknown/release/clvm_tools_rs.wasm
	npm link ./pkg
	(cd mock-test && npm link clvm_tools_rs)
