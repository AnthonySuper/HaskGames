FROM nixos/nix

RUN nix-channel --add https://nixcache.reflex-frp.org


RUN mkdir build/
COPY "." "/build/"

WORKDIR /build


RUN "nix-build" "-v"
RUN "mkdir" "-p" "bundle"
RUN "cp" "-r" "result/ghcjs/haskgames-client/bin/haskgames-client.jsexe" "bundle/public"
RUN "cp" "result/ghc/haskgames-server/bin/haskgames-server" "bundle/server"
RUN "chmod" "-R" "0777" "bundle"
RUN "chmod" "+x" "bundle/server"

WORKDIR /build/bundle

EXPOSE 80

CMD ["/bundle/server"]