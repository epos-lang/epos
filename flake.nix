{
  description = "Epos Compiler";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = import nixpkgs { inherit system; };
      in {
        packages.default = pkgs.buildGoModule {
          pname = "epos";
          version = "0.0.1";
          src = ./.;

          vendorHash = "sha256-e/xsyLXXU4sdWFiyTLTxYodCEex9GCFYuPuWG97AGHc=";
          doCheck = false;
          buildFlagsArray = [ "-ldflags=-s -w" ];
          buildInputs = with pkgs; [ go_1_25 ];
          nativeBuildInputs = with pkgs; [ makeWrapper ];
          installPhase = ''
            mkdir -p $out/bin
            export HOME=$(pwd)
            ${pkgs.go_1_25}/bin/go build -o $out/bin/epos ${self}/cmd/main.go
            wrapProgram $out/bin/epos \
              --prefix PATH : ${pkgs.lib.makeBinPath [ pkgs.llvm pkgs.clang ]}
          '';
        };

        devShells.default = pkgs.mkShell {
          packages = with pkgs; [ go_1_25 llvm clang ];
          shellHook = ''
            echo "Run './result/bin/epos *epos_file* -o *output_file*'"
          '';
        };
      });
}
