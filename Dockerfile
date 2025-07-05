FROM alpine:latest

# Required to ensure tools like ghcup and GHC build
RUN set -eux; \
    apk update && apk add --no-cache \
        curl gcc g++ git gmp-static gmp-dev libffi-dev make musl-dev libc-dev \
        zlib-static zlib-dev ncurses-static ncurses-dev xz tar perl file

# Install GHCup and toolchain non-interactively
ENV BOOTSTRAP_HASKELL_NONINTERACTIVE=1
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

# Make GHCup environment available
ENV PATH="/root/.ghcup/bin:/root/.cabal/bin:$PATH"
RUN . /root/.ghcup/env && \
    ghcup install ghc 9.10.2 && \
    ghcup set ghc 9.10.2 && \
    ghcup install cabal latest && \
    ghcup set cabal latest

# Create a working directory where source will be mounted
WORKDIR /mnt

# Default build command (override with docker run args)
CMD cabal update && \
    cabal install --enable-executable-static --enable-executable-stripping \
      --installdir=out --overwrite-policy=always --install-method=copy && \
    file out/*
