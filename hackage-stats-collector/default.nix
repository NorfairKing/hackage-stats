{ mkDerivation, base, bytestring, Cabal, conduit, containers, lib
, path, path-io, tar, text, zlib
}:
mkDerivation {
  pname = "hackage-stats-collector";
  version = "0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring Cabal conduit containers path path-io tar text zlib
  ];
  executableHaskellDepends = [ base ];
  license = "unknown";
  mainProgram = "hackage-stats-collector";
}
