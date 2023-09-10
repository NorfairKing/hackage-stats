{ mkDerivation, base, bytestring, Cabal, cassava, cassava-conduit
, conduit, containers, lib, path, path-io, tar, text, time, zlib
}:
mkDerivation {
  pname = "hackage-stats-collector";
  version = "0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring Cabal cassava cassava-conduit conduit containers
    path path-io tar text time zlib
  ];
  executableHaskellDepends = [ base ];
  license = "unknown";
  mainProgram = "hackage-stats-collector";
}
