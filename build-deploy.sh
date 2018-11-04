#!/bin/bash

nix build

rm -rf bundle 
mkdir -p bundle
cp -r result/ghcjs/haskgames-client/bin/haskgames-client.jsexe bundle/public
cp result/ghc/haskgames-server/bin/haskgames-server bundle/server
chmod -R 0777 bundle