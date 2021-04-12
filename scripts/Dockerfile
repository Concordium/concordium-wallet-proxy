# syntax=docker/dockerfile:experimental
FROM 192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium/base:0.18 as wallet-proxy-build

# Build wallet-proxy binary.
COPY . /build
WORKDIR /build
ENV STACK_ROOT /build/.stack
RUN stack build --copy-bins --ghc-options -j4 --local-bin-path target
RUN mkdir -p /bins
RUN cp target/wallet-proxy /bins/wallet-proxy

# Fetch identity-providers-with-metadata.json and global.json from genesis data.
RUN mkdir -p -m 0600 ~/.ssh && ssh-keyscan gitlab.com >> ~/.ssh/known_hosts
# Clone specified ref of genesis-data (master by default).
# The only files that are needed are `identity-providers-with-metadata.json` and `global.json`.
# These change rarely and are always manually generated and curated.
# It is important that those files match the identity providers and global parameters of the network
# the wallet-proxy will connect to. This needs to be ensured manually.
ARG genesis_ref
ARG genesis_path
WORKDIR /tmp
RUN --mount=type=ssh git clone --depth 1 --branch "${genesis_ref}" git@gitlab.com:Concordium/genesis-data.git
COPY "genesis-data/${genesis_path}/generated-data" /genesis-data

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
COPY --from=wallet-proxy-build /bins/wallet-proxy /wallet-proxy
COPY --from=wallet-proxy-build /build/deps/concordium-client/deps/concordium-base/rust-src/target/release/*.so /usr/lib/
COPY --from=wallet-proxy-build /build/scripts/start.sh /start.sh
COPY --from=wallet-proxy-build /genesis-data/global.json /wallet-proxy-data/global.json
#COPY --from=wallet-proxy-build /genesis-data/identity-providers-with-metadata.json /wallet-proxy-data/identity-providers-with-metadata.json
#RUN chmod a+x /start.sh
RUN touch client_session_key.aes && \
    chmod 0777 client_session_key.aes
USER docker
WORKDIR /
ENTRYPOINT ["/start.sh"]
