{ sources ? import ./nix/sources.nix }:
import sources.nixpkgs
  { overlays = [ ( import ./nix/overlay.nix ) ] ; config = {}; }

