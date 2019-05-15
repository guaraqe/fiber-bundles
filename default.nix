{ mkDerivation, base, containers, groups, stdenv }:
mkDerivation {
  pname = "fiber-bundles";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [ base containers groups ];
  description = "Fiber bundles and related algebraic structures";
  license = stdenv.lib.licenses.bsd3;
}
