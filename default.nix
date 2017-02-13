{ compiler ? "ghc802", profiling ? false, haddock ? false }:
  let
   config = {
     allowBroken = true;
     packageOverrides = pkgs: {
       haskell.packages.${compiler} = pkgs.haskell.packages.${compiler}.override {
          overrides = self: super: rec {
            mkDerivation = args: super.mkDerivation (args // {
              # Only run tests on stripe-http-streams
              doCheck = args.pname == "google-translate";
              doHaddock = haddock;
	      enableLibraryProfiling = profiling;
            });
            google-translate = self.callPackage ./google-translate.nix { };
          };
        };
      };
    };
   in with (import <nixpkgs> { inherit config; }).haskell.packages.${compiler}; {
     inherit google-translate;
   }
