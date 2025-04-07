{
  description = "Cloudinary client API";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/24.11";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  inputs.pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
  inputs.pre-commit-hooks.inputs.nixpkgs.follows = "nixpkgs";

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    pre-commit-hooks,
  } @ inputs:
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

        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs;
            [cabal-install ghcid ormolu nixpkgs-fmt]
            ++ self.packages.${system}.cloudinary.env.nativeBuildInputs;
        };
        apps.cloudinary-cli = {
          type = "app";
          program = "${pkgs.haskellPackages.cloudinary}/bin/cloudinary-cli";
        };
        checks = {pre-commit-check = precommitCheck;};
      }
    );
}
