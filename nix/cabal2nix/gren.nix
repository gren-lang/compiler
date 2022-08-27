{ mkDerivation, ansi-terminal, ansi-wl-pprint, base, binary, bytestring
, containers, directory, edit-distance, filelock, filepath, ghc-prim, haskeline
, hspec, hspec-discover, lib, mtl, process, raw-strings-qq, scientific, text
, time, utf8-string, vector }:
mkDerivation {
  pname = "gren";
  version = "0.2.0";
  src = ../..;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    ansi-terminal
    ansi-wl-pprint
    base
    binary
    bytestring
    containers
    directory
    edit-distance
    filelock
    filepath
    ghc-prim
    haskeline
    mtl
    process
    raw-strings-qq
    scientific
    text
    time
    utf8-string
    vector
  ];
  testHaskellDepends = [
    ansi-terminal
    ansi-wl-pprint
    base
    binary
    bytestring
    containers
    directory
    edit-distance
    filelock
    filepath
    ghc-prim
    haskeline
    hspec
    mtl
    process
    raw-strings-qq
    scientific
    text
    time
    utf8-string
    vector
  ];
  testToolDepends = [ hspec-discover ];
  homepage = "https://gren-lang.org";
  description = "The `gren` command line interface";
  license = lib.licenses.bsd3;
  mainProgram = "gren";
}
