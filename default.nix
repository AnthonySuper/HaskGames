(import ./reflex-platform {}).project ({ pkgs, ...} : {
  packages = {
    haskgames-shared = ./haskgames-shared;
    haskgames-server = ./haskgames-server;
    haskgames-client = ./haskgames-client;
  };

  shells = {
    ghc = ["haskgames-shared" "haskgames-server" "haskgames-client"];
    ghcjs = ["haskgames-shared" "haskgames-client"];
  };
})
