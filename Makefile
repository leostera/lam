.PHONY: build
build: build.wasm
	cargo build

.PHONY: build.wasm
build.wasm:
	cargo build --target wasm32-wasi --package lam-rts-wasm

.PHONY: release
release: release.wasm
	cargo build --release

.PHONY: release.wasm
release.wasm:
	cargo build --release --target wasm32-wasi --package lam-rts-wasm

.PHONY: release.rts
release.rts:
	cargo build --release --package lam-rts

.PHONY: install
install: release.wasm release.rts
	cargo install --path ./lib/lam-bin

.PHONY: clean
clean:
	cargo clean
