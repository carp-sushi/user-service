FROM fpco/stack-build:lts-18.15 as build
RUN mkdir /opt/build
COPY . /opt/build
RUN cd /opt/build && stack install --system-ghc

FROM debian:buster-slim
ARG BINARY_PATH
WORKDIR /usr/local/bin
RUN apt-get update && apt-get install -y ca-certificates libgmp-dev
RUN rm -rf /var/lib/apt/lists/
COPY --from=build /root/.local/bin/user-service-exe .
COPY --from=build /opt/build/user-service.cfg .
CMD ["/usr/local/bin/user-service-exe"]
