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
        packages.default = pkgs.stdenv.mkDerivation {
          pname = "lua_llvm";
          version = "0.0.1";
          src = ./.;

          buildInputs = with pkgs; [ go llvm clang ];

          buildPhase = ''
            mkdir -p $out/bin
            ${pkgs.go}/bin/go build ./cmd -o $out/bin/lua_llvm
          '';
        };

        devShells.default = pkgs.mkShell {
          packages = with pkgs; [ go llvm clang ];
          shellHook = ''
            #echo ""
          '';
        };
      });
}
