self : super:
  { haskellPackages = super.haskellPackages.override
      { overrides = _ : pkgs:
          { cloudinary-types = pkgs.callCabal2nix "cloudinary-types" ../cloudinary-types {}; 
            cloudinary-io = pkgs.callCabal2nix "cloudinary-io" ../cloudinary-io {}; 
          };
      };
  }

