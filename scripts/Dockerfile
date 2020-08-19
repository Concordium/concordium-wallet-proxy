# syntax=docker/dockerfile:experimental
FROM 192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium/base-haskell:0.14 as wallet-proxy-build
COPY . /build
WORKDIR /build
ENV STACK_ROOT /build/.stack
RUN mkdir -p -m 0600 ~/.ssh && ssh-keyscan gitlab.com >> ~/.ssh/known_hosts
RUN --mount=type=ssh git clone git@gitlab.com:Concordium/p2p-client.git
RUN --mount=type=ssh (cd p2p-client && git checkout develop && ./scripts/download-genesis-complementary-bundle.sh)
RUN ( cd deps/simple-client && ./build-deps.sh )
RUN mkdir -p /libs
RUN cp deps/simple-client/extra-libs/*.so /libs
ENV LD_LIBRARY_PATH=/libs
RUN stack build --copy-bins --ghc-options -j4 --local-bin-path target
RUN mkdir -p /bins
RUN cp target/wallet-proxy-exe /bins/wallet-proxy
RUN cp -r p2p-client/genesis-complementary-bundle /genesis-complementary-bundle

FROM ubuntu:20.04
RUN apt-get update && apt-get install -y unbound curl postgresql-server-dev-12 liblmdb0
RUN useradd -l -m -s /bin/false -u 61000 docker
COPY --from=wallet-proxy-build /bins/wallet-proxy /wallet-proxy
COPY --from=wallet-proxy-build /libs/* /usr/lib/
COPY --from=wallet-proxy-build /build/scripts/start.sh /start.sh
RUN mkdir -p /wallet-proxy-data
COPY --from=wallet-proxy-build /genesis-complementary-bundle/identity-providers-with-metadata.json /wallet-proxy-data/identity-providers-with-metadata.json
COPY --from=wallet-proxy-build /genesis-complementary-bundle/global.json /wallet-proxy-data/global.json
RUN chmod a+x /start.sh
RUN touch client_session_key.aes
RUN chmod 0777 client_session_key.aes
USER docker
WORKDIR /
ENTRYPOINT ["/start.sh"]
