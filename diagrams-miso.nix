{ mkDerivation, base, colour, containers, diagrams-core
, diagrams-lib, lens, miso, monoid-extras, mtl, stdenv
}:
mkDerivation {
  pname = "diagrams-miso";
  version = "0.1";
  src = ./.;
  libraryHaskellDepends = [
    base colour containers diagrams-core diagrams-lib lens miso
    monoid-extras mtl
  ];
  homepage = "https://github.com/cocreature/diagrams-miso";
  description = "reflex backend for diagrams drawing EDSL";
  license = stdenv.lib.licenses.bsd3;
}
