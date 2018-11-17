#!/bin/bash

export PATH="$patchelf/bin:$coreutils/bin:$binutils/bin:$findutils/bin"
mkdir $out
cp -a "$1/." "$out/"
find $out -type f -exec patchelf --shrink-rpath '{}' \; -exec strip '{}' \;
