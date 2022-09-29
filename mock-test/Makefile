.PHONY: runmock.js

all: runmock.js

runmock.js:
	cd .. && cargo build --target wasm32-unknown-unknown && wasm-bindgen --web --out-dir ../pkg ./target/wasm32-unknown-unknown/debug/clvm_tools_rs.wasm
	./node_modules/.bin/webpack

run: runmock.js
	node ./build/runmock.js
