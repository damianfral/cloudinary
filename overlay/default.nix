final : prev:
  { haskellPackages = prev.haskellPackages.override
      { overrides = _ : pkgs:
          { cloudinary = pkgs.callCabal2nix "cloudinary" ../. {};
          };
        };
  }

