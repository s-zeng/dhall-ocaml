{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/refs/tags/22.11.tar.gz") {} }:

# pkgs.mkShell {
#   nativeBuildInputs = [ pkgs.opam ];
#   buildInputs = with pkgs; [
#     dune_3
#     ocaml
#     ocamlPackages.core
#     ocamlPackages.ppx_jane
#     ocamlPackages.higher_kinded
#   ];
# }

pkgs.mkShell {
  buildInputs = with pkgs; [
    dune_3
    ocaml
    ocamlPackages.angstrom
    ocamlPackages.bignum
    ocamlPackages.core
    ocamlPackages.findlib
    ocamlPackages.higher_kinded
    ocamlPackages.ppx_jane
  ];
  packages = with pkgs; [
    ocamlPackages.ocaml-lsp
    ocamlformat
  ];
}

