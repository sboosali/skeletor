{ mkDerivation, attoparsec, base, base16-bytestring, bytestring
, case-insensitive, containers, cryptohash-sha256, deepseq, dhall
, directory, filemanip, filepath, generic-lens, hashable, hedgehog
, http-client, http-client-tls, http-types, HUnit, lens, modern-uri
, mtl, optparse-applicative, spiros, stdenv, tar, tasty
, tasty-hedgehog, tasty-hunit, temporary, text, these, transformers
, trifecta, turtle, typed-process, unordered-containers, zlib
}:
mkDerivation {
  pname = "skeletor";
  version = "0.0.0";
  src = ./skeletor;
  configureFlags = [ "-fstatic" ];
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    attoparsec base base16-bytestring bytestring case-insensitive
    containers cryptohash-sha256 deepseq dhall directory filemanip
    filepath generic-lens hashable http-client http-client-tls
    http-types lens modern-uri mtl spiros tar temporary text these
    transformers trifecta turtle typed-process unordered-containers
    zlib
  ];
  executableHaskellDepends = [
    attoparsec base case-insensitive generic-lens optparse-applicative
    spiros text
  ];
  testHaskellDepends = [
    base hedgehog HUnit tasty tasty-hedgehog tasty-hunit
  ];
  doCheck = false;
  homepage = "http://github.com/sboosali/skeletor#readme";
  description = "Haskell project scaffolding";
  license = stdenv.lib.licenses.gpl3Plus;
}
