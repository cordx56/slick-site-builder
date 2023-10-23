FROM haskell:9.4.7 AS builder
WORKDIR /app
COPY package.yaml .
COPY stack.yaml .
COPY stack.yaml.lock .
RUN stack install --only-dependencies
COPY . .
RUN stack install --local-bin-path ./

FROM debian:buster-slim
WORKDIR /app
COPY --from=builder /app/build-site /
CMD ["/build-site"]
