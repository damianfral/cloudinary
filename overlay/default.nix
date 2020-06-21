final : prev:
{
  haskellPackages = prev.haskellPackages.override
  {
    overrides
      = prev.lib.composeExtensions (prev.haskellPackages.overrides or (_: _: {}))
        (hself: hsuper:
        { cloudinary = hsuper.callCabal2nix "cloudinary" ../. {};
        });
      };
}

