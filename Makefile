all:
	cargo build --release --no-default-features
	cargo build --release --target wasm32-unknown-unknown
	cd wasm && wasm-pack build && npm link ./pkg
