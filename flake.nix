{
  description = "Cloudinary client API";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/24.11";
    flake-utils.url = "github:numtide/flake-utils";
    pre-commit-hooks = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    pre-commit-hooks,
  }:
    {overlay = import ./overlay;}
    // flake-utils.lib.eachDefaultSystem (
      system: let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [self.overlay];
        };
        precommitCheck = pre-commit-hooks.lib.${system}.run {
          src = ./.;
          hooks = {
            actionlint.enable = true;
            alejandra.enable = true;
            deadnix.enable = true;
            hlint.enable = true;
            hpack.enable = true;
            markdownlint.enable = true;
            nil.enable = true;
            ormolu.enable = true;
            statix.enable = true;
          };
        };
      in rec {
        packages.cloudinary = with pkgs.haskellPackages; cloudinary;
        packages.default = packages.cloudinary;

        devShells.default = pkgs.haskellPackages.shellFor {
          packages = p: [p.cloudinary];
          nativeBuildInputs = with pkgs;
          with pkgs.haskellPackages; [
            # actionlint
            alejandra
            cabal-install
            feedback
            ghcid
            haskell-language-server
            hlint
            markdown-oxide
            nil
            ormolu
            statix
            yaml-language-server
          ];
          inherit (precommitCheck) shellHook;
        };
        apps.cloudinary-cli = {
          type = "app";
          program = "${pkgs.haskellPackages.cloudinary}/bin/cloudinary-cli";
        };
        checks = {pre-commit-check = precommitCheck;};
      }
    );
}
