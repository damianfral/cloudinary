{
  description = "Cloudinary client API";


  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-20.09";
  inputs.utils.url   = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, utils }@inputs:

    let
      system = "x86_64-linux";
      pkgs   = import nixpkgs
      { inherit system;
        overlays = [ self.overlay ];
      };

    in
      {
        overlay = import ./overlay;

        packages.${system} =
          with pkgs.haskellPackages;
          { inherit cloudinary; };

        devShell.${system}       = self.packages.${system}.cloudinary.env;
        defaultPackage.${system} = self.packages.${system}.cloudinary;
        apps.${system} =
          { cloudinary-cli = {
              type = "app";
              program = "${pkgs.haskellPackages.cloudinary}/bin/cloudinary-cli";
            };
          };

        defaultApp.${system} = self.apps.${system}.cloudinary-cli;
      };
}
