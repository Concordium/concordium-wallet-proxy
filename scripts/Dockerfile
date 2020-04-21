FROM 192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium/base-haskell:0.10 as wallet-proxy-build

COPY . /build

WORKDIR /build

RUN ( cd deps/simple-client && ./build-deps.sh )

RUN mkdir -p /libs

RUN cp deps/simple-client/extra-libs/*.so /libs

ENV LD_LIBRARY_PATH=/libs

RUN stack build --copy-bins --ghc-options -j4 --local-bin-path target

RUN mkdir -p /bins

RUN cp target/wallet-proxy-exe /bins/wallet-proxy


FROM ubuntu:19.10

RUN apt-get update && apt-get install -y unbound curl postgresql-server-dev-11 liblmdb0

RUN useradd -l -m -s /bin/false -u 61000 docker

COPY --from=wallet-proxy-build /bins/wallet-proxy /wallet-proxy
COPY --from=wallet-proxy-build /libs/* /usr/lib/
COPY --from=wallet-proxy-build /build/scripts/start.sh /start.sh

RUN chmod a+x /start.sh

RUN touch client_session_key.aes
RUN chmod 0777 client_session_key.aes

USER docker

WORKDIR /

ENTRYPOINT ["/start.sh"]