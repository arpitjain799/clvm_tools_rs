all:
	cargo build --release
	cargo build --release --target wasm32-unknown-unknown
	wasm-pack build
	wasm-bindgen --web --out-dir ./pkg ./target/wasm32-unknown-unknown/release/clvm_tools_rs.wasm
	npm link ./pkg
	(cd mock-test && npm link clvm_tools_rs)
