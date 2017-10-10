{ mkDerivation, aeson, base, bytestring, containers, ghcjs-base
, http-api-data, http-types, network-uri, scientific, servant
, stdenv, text, transformers, unordered-containers, vector
}:
mkDerivation {
  pname = "miso";
  version = "0.9.0.0";
  sha256 = "3ffc757cfa860b9efa54ab1df91a52750fb92a34df98652dcc4216d30226a326";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring containers ghcjs-base http-api-data
    http-types network-uri scientific servant text transformers
    unordered-containers vector
  ];
  homepage = "http://github.com/dmjio/miso";
  description = "A tasty Haskell front-end framework";
  license = stdenv.lib.licenses.bsd3;
}
