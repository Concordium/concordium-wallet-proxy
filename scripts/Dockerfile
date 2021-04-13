ARG base_image_tag
FROM concordium/base:${base_image_tag} as build

# Build wallet-proxy binary.
COPY . /build
WORKDIR /build
ENV STACK_ROOT /build/.stack
RUN stack build --copy-bins --ghc-options -j4 --local-bin-path target
RUN mkdir -p /bins
RUN cp target/wallet-proxy /bins/wallet-proxy

# Collect build artifacts in fresh image.
FROM ubuntu:20.04
RUN apt-get update && \
    apt-get -y install \
      unbound \
      curl \
      postgresql-server-dev-12 \
      liblmdb0 \
    && rm -rf /var/lib/apt/lists/*
RUN useradd -l -m -s /bin/false -u 61000 docker
COPY --from=build /bins/wallet-proxy /wallet-proxy
COPY --from=build /build/deps/concordium-client/deps/concordium-base/rust-src/target/release/*.so /usr/lib/
COPY --from=build /build/scripts/start.sh /start.sh
RUN touch client_session_key.aes && \
    chmod 0777 client_session_key.aes
USER docker
WORKDIR /
ENTRYPOINT ["/start.sh"]
