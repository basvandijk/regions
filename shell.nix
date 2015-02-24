let
  pkgs = import <nixpkgs> {};
  haskellPackages = pkgs.haskellPackages.override {
    extension = self: super: {
      regions = self.callPackage ./regions.nix {};
    };
  };

in pkgs.myEnvFun {
     name = haskellPackages.regions.name;
     buildInputs = [
       (haskellPackages.ghcWithPackages (hs: ([
         hs.cabalInstall
         hs.hscolour
       ] ++ hs.regions.propagatedNativeBuildInputs)))
     ];
   }
