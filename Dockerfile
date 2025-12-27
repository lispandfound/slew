FROM alpine:latest

# Required to ensure tools like ghcup and GHC build
RUN set -eux; \
    apk update && apk add --no-cache \
        curl gcc g++ git gmp-static gmp-dev libffi-dev make musl-dev libc-dev \
        zlib-static zlib-dev ncurses-static ncurses-dev xz tar perl file

# Install GHCup and toolchain non-interactively
ENV BOOTSTRAP_HASKELL_NONINTERACTIVE=1
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
ARG GHC_VERSION=9.10.2
# Make GHCup environment available
ENV PATH="/root/.ghcup/bin:/root/.cabal/bin:$PATH"
RUN . /root/.ghcup/env && \
    ghcup install ghc ${GHC_VERSION} && \
    ghcup set ghc ${GHC_VERSION} && \
    ghcup install cabal latest && \
    ghcup set cabal latest
