with import <nixpkgs> {};
let
  my-nixpkgs = import <nixpkgs> {};
  my-proj = import ./default.nix;
in stdenv.mkDerivation {
  name = "haskgames-server-dist";
  builder = "${my-nixpkgs.bash}/bin/bash/";
  args = [ ./patch_binary.sh "${my-proj.ghc.haskgames-server}" "${patchelf}/bin/patchelf" ];
  patchelf = patchelf;
  coreutils = coreutils;
  binutils = binutils;
  findutils = findutils;
}
