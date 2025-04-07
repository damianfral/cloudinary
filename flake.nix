{
  description = "Cloudinary client API";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/24.11";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = {
    self,
    nixpkgs,
    flake-utils,
  } @ inputs:
    {overlay = import ./overlay;}
    // flake-utils.lib.eachDefaultSystem (
      system: let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [self.overlay];
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
      }
    );
}
