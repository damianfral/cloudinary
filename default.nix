{ sources ? import ./nix/sources.nix }:
with
  { overlay = _: pkgs: rec
      { inherit (import sources.niv {}) niv;
        haskellPackages = pkgs.haskellPackages.override
          { overrides = _: super:
              { cloudinary-types = super.callCabal2nix "cloudinary-types" ./cloudinary-types {}; 
                cloudinary-io = super.callCabal2nix "cloudinary-io" ./cloudinary-io {}; 
              };
          };
      };
  };
import sources.nixpkgs
  { overlays = [ overlay ] ; config = {}; }

