with import <nixpkgs> { };
let
  i = import ./default.nix;
  b = import ./build-stripped.nix;
  
in 
(import <nixpkgs> {}).dockerTools.buildImage {
  name = "haskgames-server-software";
  contents = [ b i.ghcjs.haskgames-client pkgs.bashInteractive coreutils binutils ];
  config = {
    Cmd = [ "${b}/bin/haskgames-server" "-p" "80" ];
    ExposedPorts = { 
      "80" = {};
    };
  };
}
