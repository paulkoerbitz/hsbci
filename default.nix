{ haskellPackages ? (import <nixpkgs> {}).haskellPackages }:
let
  inherit (haskellPackages) 
    cabal 
    httpConduit 
    HUnit 
    mtl 
    testFramework 
    testFrameworkHunit
    testFrameworkQuickcheck2 
    text 
    vector
    xml;
in

cabal.mkDerivation (self: {
  pname = "hsbci";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    httpConduit HUnit mtl testFramework testFrameworkHunit
    testFrameworkQuickcheck2 text vector xml
  ];
  testDepends = [
    HUnit mtl testFramework testFrameworkHunit testFrameworkQuickcheck2
    text vector xml
  ];
  meta = {
    description = "A fast, simple and modular HBCI library for Haskell";
    platforms = self.ghc.meta.platforms;
  };
  enableSplitObjs = false;
})
