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
    xml
    base64Bytestring
    prettyShow;
in

cabal.mkDerivation (self: {
  pname = "hsbci";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    httpConduit HUnit mtl testFramework testFrameworkHunit
    testFrameworkQuickcheck2 text vector xml prettyShow
  ];
  testDepends = [
    HUnit mtl testFramework testFrameworkHunit testFrameworkQuickcheck2
    text vector xml prettyShow base64Bytestring
  ];
  meta = {
    description = "A fast, simple and modular HBCI library for Haskell";
    platforms = self.ghc.meta.platforms;
  };
  enableSplitObjs = false;
})
