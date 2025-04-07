{
  description = "Cloudinary client API";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/24.11";
  inputs.utils.url = "github:numtide/flake-utils";

  outputs = {
    self,
    nixpkgs,
    utils,
  } @ inputs: let
    system = "x86_64-linux";
    pkgs =
      import nixpkgs
      {
        inherit system;
        overlays = [self.overlay];
      };
  in {
    overlay = import ./overlay;

    packages.${system} = with pkgs.haskellPackages; {inherit cloudinary;};

    devShell.${system} = pkgs.mkShell {
      buildInputs = with pkgs;
        [cabal-install ghcid ormolu nixpkgs-fmt]
        ++ self.packages.${system}.cloudinary.env.nativeBuildInputs;
    };
    defaultPackage.${system} = self.packages.${system}.cloudinary;
    apps.${system} = {
      cloudinary-cli = {
        type = "app";
        program = "${pkgs.haskellPackages.cloudinary}/bin/cloudinary-cli";
      };
    };

    defaultApp.${system} = self.apps.${system}.cloudinary-cli;
    checks.${system} = {
      build = self.defaultPackage.${system};
    };
  };
}
