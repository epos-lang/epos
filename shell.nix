{ pkgs ? import <nixpkgs> { } }:

pkgs.mkShell {
  packages = with pkgs; [ go llvm clang ];
  shellHook = ''
    tmpdir=$(mktemp -d)
    ${pkgs.go}/bin/go build -o $tmpdir/epos ./cmd/main.go
    export PATH="$tmpdir:$PATH"
    echo "Run 'epos [-r] <epos_file> [-o <output_file>]'"
  '';
}
