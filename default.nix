{ haskellPackages ? (import <nixpkgs> {}).haskellPackages }:
let
  inherit (haskellPackages)
    dataDefaultInstancesOldLocale
    cabal
    cabalInstall
    httpConduit
    HUnit
    mtl
    testFramework
    testFrameworkHunit
    testFrameworkQuickcheck2
    text
    time
    vector
    xml
    base64Bytestring
    prettyShow
    ghcMod;
in

cabal.mkDerivation (self: {
  pname = "hsbci";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    httpConduit HUnit mtl testFramework testFrameworkHunit
    testFrameworkQuickcheck2 text time vector xml prettyShow
    ghcMod cabalInstall dataDefaultInstancesOldLocale
  ];
  testDepends = [
    HUnit mtl testFramework testFrameworkHunit testFrameworkQuickcheck2
    text time vector xml prettyShow base64Bytestring cabalInstall
    dataDefaultInstancesOldLocale
  ];
  meta = {
    description = "A fast, simple and modular HBCI library for Haskell";
    platforms = self.ghc.meta.platforms;
  };
  enableSplitObjs = false;
})
