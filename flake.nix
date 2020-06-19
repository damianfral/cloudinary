{
  description = "Cloudinary client API";


  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-20.03";
  inputs.utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, utils }:


  # utils.lib.eachDefaultSystem (system:

    let
      system = "x86_64-linux";
      pkgs = import nixpkgs
      { inherit system;
        overlays = [ self.overlay ];
      };

    in
      {
        overlay = import ./overlay;
        packages.${system} = with pkgs.haskellPackages;
          { inherit cloudinary-types cloudinary-io;
        };
        # packages.${system}.cloudinary-types = pkgs.haskellPackages.cloudinary-types;
        defaultPackage.${system} = self.packages.${system}.cloudinary-types;
        # apps.${system} = pkgs.cloudinary-types;
          # packages.${system}.cloudinary-types;
        # apps.hello = utils.lib.mkApp { drv = packages.hello; };
        # defaultApp = apps.hello;
      };
    # );
}
