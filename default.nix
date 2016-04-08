{ mkDerivation, base, containers, hashable, logict, mtl, stdenv
, text, transformers, unordered-containers, vector
, vector-algorithms
}:
mkDerivation {
  pname = "access-rolebased";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base containers hashable logict mtl text transformers
    unordered-containers vector vector-algorithms
  ];
  doCheck = false;
  description = "RBAC checker";
  license = stdenv.lib.licenses.bsd3;
}
