let
  pkgs = import <nixpkgs> {};
  haskellPackages = pkgs.haskellPackages.override {
    extension = self: super: {
      storableSop = self.callPackage ./. {};
    };
  };

in pkgs.myEnvFun {
     name = haskellPackages.storableSop.name;
     buildInputs = [
       pkgs.curl
       (haskellPackages.ghcWithPackages (hs: ([
         hs.cabalInstall
         hs.hscolour
       ] ++ hs.storableSop.propagatedNativeBuildInputs)))
     ];
   }