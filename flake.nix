{

  inputs = {
    miso.url = "github:dmjio/miso/1.12.0";
  };

  outputs = inputs:
    inputs.miso.inputs.flake-utils.lib.eachDefaultSystem (system: {
      devShell = inputs.miso.outputs.devShells.${system}.default;
      devShells.hls = inputs.miso.outputs.devShells.${system}.hls;
      devShells.wasm = inputs.miso.outputs.devShells.${system}.wasm;
      devShells.ghcjs = inputs.miso.outputs.devShells.${system}.ghcjs;
    });

}

