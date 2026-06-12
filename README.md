🧊 miso-blockout
================

A [Blockout](https://en.wikipedia.org/wiki/Blockout) (3D Tetris) clone written in Haskell
using [miso](https://github.com/dmjio/miso), compiled to WebAssembly.

### Prerequisites

[Nix](https://nixos.org) with [flakes](https://wiki.nixos.org/wiki/Flakes) enabled.

### Build (Web Assembly)

```bash
nix develop .#wasm --command bash -c "make"
```

### Serve

```bash
nix develop .#wasm --command bash -c "make serve"
```

Then open http://localhost:8080.

### Interactive development

```bash
nix develop .#wasm --command bash -c "make repl"
```

Open the printed URL in the browser, then run `main` in the REPL.
