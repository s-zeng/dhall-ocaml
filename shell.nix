{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/refs/tags/22.11.tar.gz") {} }:

pkgs.mkShell {
  packages = with pkgs; [
    dune_3
    ocaml
    ocamlPackages.core
    ocamlPackages.ppx_jane
    ocamlPackages.higher_kinded
  ];
}

