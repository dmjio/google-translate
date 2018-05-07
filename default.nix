{ compiler ? "ghc822" }:
  let
   config = {
     packageOverrides = pkgs: with pkgs.haskell.lib; {
       haskell = pkgs.haskell // {
         packages = pkgs.haskell.packages // {
           ${compiler} = pkgs.haskell.packages.${compiler}.override {
             overrides = self: super: rec {
               google-translate = buildStrictly (self.callPackage ./google-translate.nix {});
             };
           };
         };
       };
     };
   };
   in
     let
       pkgs = import <nixpkgs> { inherit config; };
       google-translate = pkgs.haskell.packages.${compiler}.google-translate;
     in { inherit google-translate; }

