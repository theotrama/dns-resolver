# Build stage 0
FROM erlang:26-alpine

# Set working directory
RUN mkdir /buildroot
WORKDIR /buildroot

# Copy our Erlang test application
COPY . domain_name_resolver

# And build the release
WORKDIR domain_name_resolver
RUN rebar3 as prod release

# Build stage 1
FROM alpine

# Install some libs
RUN apk add --no-cache openssl && \
    apk add --no-cache ncurses-libs && \
    apk add --no-cache libstdc++

# Install the released application
COPY --from=0 /buildroot/domain_name_resolver/_build/prod/rel/domain_name_resolver /domain_name_resolver

# Expose relevant ports
EXPOSE 8080

CMD ["/domain_name_resolver/bin/domain_name_resolver", "foreground"]
