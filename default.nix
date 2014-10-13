{ cabal, genericsSop }:

cabal.mkDerivation (self: {
  pname = "storable-sop";
  version = "1.0.0";
  sha256 = "nil";
  buildDepends = [ genericsSop ];
  meta = {
    homepage = "http://github.com/ocharles/storable-sop";
    description = "A generic implementation of Storable";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
