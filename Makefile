LAM_EXE=lam

.PHONY: build
build: build.wasm build.web
	cargo build

.PHONY: build.wasm
build.wasm:
	cargo build --target wasm32-wasi --package lam-rts-wasm

.PHONY: build.web
build.web:
	wasm-pack build \
		--dev \
		--target web \
		--no-typescript \
		./lib/lam-rts-web \
		-- --package lam-rts-web

.PHONY: docs
docs:
	cargo doc --target-dir ./docs --workspace --no-deps

.PHONY: test
test:
	cargo test

.PHONY: release.win
release.win: LAM_EXE=lam.exe
release.win: release

.PHONY: release
release: release.wasm release.web
	cargo build --release
	tar czf release.tar.gz -C ./target/release/ $(LAM_EXE)

.PHONY: release.wasm
release.wasm:
	cargo build --release --target wasm32-wasi --package lam-rts-wasm

.PHONY: release.web
release.web:
	wasm-pack build \
		--release \
		--target web \
		--no-typescript \
		./lib/lam-rts-web \
		-- --package lam-rts-web

.PHONY: install
install: release
	cargo install --path ./lib/lam-bin

.PHONY: setup
setup:
	cargo install wasm-pack
	rustup target add wasm32-wasi
	rustup target add wasm32-unknown-unknown

.PHONY: clean
clean:
	cargo clean

.PHONY: fmt
fmt:
	cargo fmt
