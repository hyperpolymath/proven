{ pkgs ? import (builtins.fetchGit {
    url = "https://github.com/NixOS/nixpkgs.git";
    rev = "be5afa0fcb31f0a96bf9ecba05a516c66fcd8114";
  }) {} }:

pkgs.mkShell {
  packages = with pkgs; [
    idris2
    zig
    git
    gnumake
    pkg-config
  ];
}
