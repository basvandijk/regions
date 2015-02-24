{ cabal, monadControl, transformers }:

cabal.mkDerivation (self: {
  pname = "regions";
  version = "HEAD";
  src = ./.;
  buildDepends = [ monadControl transformers ];
  meta = {
    homepage = "https://github.com/basvandijk/regions/";
    description = "Provides the region monad for safely opening and working with scarce resources";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
