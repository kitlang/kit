{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  haskellPackagesRaw = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  haskellPackages = haskellPackagesRaw.override {
    overrides = self: super: {
      inline-c = let
        srcRaw = pkgs.fetchFromGitHub
          {
            owner = "fpco";
            repo = "inline-c";
            rev = "8bae45eb7c308023965a6f069170b31b1acf4321";
            sha256 = "0scsgjdpv44cvfgm6h5ydjkwkdjrm2j21nckif19zhkx2lvjbq4l";
          };

        src = pkgs.stdenv.mkDerivation {
          name = "inline-c-src";
          buildCommand = ''
            cp -rv ${srcRaw}/inline-c $out
          '';
        };

       in super.callCabal2nix "inline-c" src {};
    };
  };

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage ./clang-pure.nix {});

in

  if pkgs.lib.inNixShell then drv.env else drv