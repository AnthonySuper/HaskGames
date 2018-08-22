(import ./reflex-platform {}).project ({ pkgs, ...} : {
  packages = {
    haskgames-shared = ./haskgames-shared;
  };

  shells = {
    ghc = ["haskgames-shared"];
    ghcjs = ["haskgames-shared"];
  };
})
