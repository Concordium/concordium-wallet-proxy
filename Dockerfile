ARG base_image_tag
FROM concordium/base:${base_image_tag} as build

# Build wallet-proxy binary.
COPY . /build
WORKDIR /build
RUN STACK_ROOT=/build/.stack stack build --copy-bins --ghc-options="-j4" --local-bin-path=target

# Collect build artifacts in fresh image.
FROM ubuntu:22.04
RUN apt-get update && \
    apt-get -y install \
      less \
      jq \
      curl \
      unbound \
      postgresql-server-dev-12 \
      liblmdb0 \
    && rm -rf /var/lib/apt/lists/*
COPY --from=build /build/target/wallet-proxy /wallet-proxy
COPY --from=build /build/deps/concordium-client/deps/concordium-base/rust-src/target/release/*.so /usr/lib/
COPY ./docker /
RUN touch client_session_key.aes && \
    chmod 0777 client_session_key.aes && \
    useradd -l -m -s /bin/false -u 61000 docker
USER docker
ENTRYPOINT ["/docker-entrypoint.sh"]
