#!/bin/sh
set -eux


apk update
apk add curl gcc g++ git gmp-dev libffi-dev make musl-dev libc-dev \
    zlib-static zlib-dev ncurses-static ncurses-dev xz tar perl file

curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | \
  BOOTSTRAP_HASKELL_NONINTERACTIVE=1 sh
. "$HOME/.ghcup/env"

# Install and set GHC and Cabal
ghcup install ghc 9.10.2
ghcup set ghc 9.10.2
ghcup install cabal latest
ghcup set cabal latest

cd /mnt

# Install the executable statically and stripped
cabal update
cabal install --enable-executable-static --enable-executable-stripping --installdir=out --overwrite-policy=always --install-method=copy

# Confirm binary properties
file out/*
