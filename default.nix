(import ./reflex-platform {}).project ({ pkgs, ...} : {
  packages = {
    haskgames-shared = ./haskgames-shared;
    haskgames-server = ./haskgames-server;
  };

  shells = {
    ghc = ["haskgames-shared" "haskgames-server"];
    ghcjs = ["haskgames-shared"];
  };
})
