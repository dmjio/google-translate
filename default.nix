{ compiler ? "ghc802" }:
  let
   config = {
     packageOverrides = pkgs: {
       haskell.packages.${compiler} = pkgs.haskell.packages.${compiler}.override {
          overrides = self: super: rec {
            google-translate = self.callPackage ./google-translate.nix { };
          };
        };
      };
   };
   in
     let
       pkgs = import <nixpkgs> { inherit config; };
       google-translate = pkgs.haskell.packages.${compiler}.google-translate;
     in { inherit google-translate; }

