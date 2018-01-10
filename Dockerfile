FROM haskell:8.0.2 as builder

RUN mkdir -p /app
WORKDIR /app

COPY stack.yaml *.cabal /app/
RUN stack build --dependencies-only

COPY . /app/
RUN stack install

FROM alpine:edge

RUN \
    apk add \
    -X http://dl-cdn.alpinelinux.org/alpine/edge/testing \
    --update --no-cache \
    libstdc++ zlib gmp ca-certificates gcompat


COPY --from=builder /root/.local/bin/tgm-breakfast-hs-exe /
CMD ["/tgm-breakfast-hs-exe"]
