

(
  import (fetchTarball https://github.com/edolstra/flake-compat/archive/master.tar.gz) {
    src = builtins.fetchGit ./.;
  }
).shellNix

# let
#   project = (import ./.).haskellPackages.server;
# in
#   pkgs.mkShell {
#     buildInputs = project.env.nativeBuildInputs ++ [
#       # pkgs.haskellPackages.cabal-install
#       # pkgs.haskellPackages.cabal-helper
#       # pkgs.haskellPackages.ghcide
#       # pkgs.haskellPackages.apply-refact
#       # pkgs.haskellPackages.hasktags
#       # pkgs.haskellPackages.hindent
#       pkgs.haskellPackages.hlint
#       pkgs.haskellPackages.hoogle
#       pkgs.haskellPackages.brittany
#     ];
# }


# { pkgs }:
# with pkgs;
# mkShell {
#   buildInputs = [
#     haskellPackages.hlint
#     haskellPackages.hoogle
#     haskellPackages.brittany
#   ];
# }A

# { pkgs ? import <nixpkgs> { } }:
# with pkgs;
# mkShell {
#   buildInputs =
#     ( import ./default.nix).haskellPackages.cloudinary-io.env.nativeBuildInputs;
#     # nixpkgs-fmt
#     # cabal-install
#   # ];

#   shellHook = ''
#     # ...
#   '';
# }
