{ mkDerivation, aeson, base, configurator, hspec, hspec-wai
, http-types, lib, monad-logger, persistent, persistent-sqlite
, persistent-template, Spock, tasty, tasty-hspec, text, validation
, wai
}:
mkDerivation {
  pname = "user-service";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base configurator http-types monad-logger persistent
    persistent-sqlite persistent-template Spock text validation wai
  ];
  executableHaskellDepends = [ aeson base Spock text ];
  testHaskellDepends = [
    base hspec hspec-wai Spock tasty tasty-hspec text validation wai
  ];
  homepage = "https://github.com/carp-sushi/user-service#readme";
  license = lib.licenses.bsd3;
  mainProgram = "user-service-exe";
}
