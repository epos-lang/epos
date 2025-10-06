{
  description = "Lua LLVM";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = import nixpkgs { inherit system; };
      in {
        packages.default = pkgs.buildGoModule {
          pname = "lua_llvm";
          version = "0.0.1";
          src = ./.;

          vendorHash = "sha256-Pz5JhdYP8ckJ2dLiN3jDJWIL/egoCgCyi8CducyY41Y=";
          doCheck = false;
          buildFlagsArray = [ "-ldflags=-s -w" ];
          buildInputs = with pkgs; [ go_1_25 llvm clang ];
          nativeBuildInputs = with pkgs; [ makeWrapper ];
          installPhase = ''
            mkdir -p $out/bin
            export HOME=$(pwd)
            ${pkgs.go_1_25}/bin/go build -o $out/bin/lua_llvm ${self}/cmd/main.go
          '';
        };

        devShells.default = pkgs.mkShell {
          packages = with pkgs; [ go_1_25 llvm clang ];
          shellHook = ''
            echo "Run './result/bin/lua_llvm *lua_file* -o *output_file*'"
          '';
        };
      });
}
