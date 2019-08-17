{ sources ? import ./nix/sources.nix }:     # import the sources
with
  { overlay = _: pkgs:
      { niv = import sources.niv {};    # use the sources :)
      };
  };

import sources.nixpkgs                  # and use them again!
  { overlays = 
    [ (pkgs : _ : {
        cloudinary-types = pkgs.haskellPackages.callCabal2nix "cloudinary-types" ./cloudinary-types {} ;
        cloudinary-io = pkgs.haskellPackages.callCabal2nix "cloudinary-io" ./cloudinary-io {} ;
      })
       
    ] 
  ; config = {}; 
}

