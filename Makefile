all:
	cargo build --release
	cargo build --release --target wasm32-unknown-unknown
	wasm-pack build
	npm link ./pkg
	(cd mock-test && npm link clvm_tools_rs)
