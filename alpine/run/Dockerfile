FROM alpine:3.3

RUN apk add --no-cache ncurses-libs && \
    apk add --no-cache libcrypto1.0

RUN apk add --no-cache gdb

# Install the released application
COPY artifacts/dockerwatch /dockerwatch

ENV ERL_CRASH_DUMP /core/erl_crash.dump

# Expose relevant ports
EXPOSE 8080
EXPOSE 8443

CMD /bin/sh -c "/dockerwatch/bin/dockerwatch console"
