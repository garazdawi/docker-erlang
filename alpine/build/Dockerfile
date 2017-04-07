FROM alpine:3.3

# Install additional packages
RUN apk add --no-cache autoconf && \
    apk add --no-cache alpine-sdk && \
    apk add --no-cache openssl-dev && \
    apk add --no-cache ncurses-dev

# Build Erlang/OTP
ADD ./otp.tar.gz /buildroot/

WORKDIR /buildroot/otp/

ENV MAKEFLAGS -j40

ENV ERL_TOP /buildroot/otp

# RUN ./otp_build autoconf && ./configure --prefix=/buildroot/erlang/20.0 --disable-hipe --enable-lock-counter
RUN ./otp_build autoconf && ./configure --prefix=/buildroot/erlang/20.0 --disable-hipe

# Install Erlang/OTP
RUN mkdir -p /buildroot/erlang/20.0 && make

RUN make install

RUN apk add --no-cache git

ENV PATH=/buildroot/erlang/20.0/bin:$PATH

# Build Rebar3
WORKDIR /buildroot/
RUN git clone http://github.com/erlang/rebar3 && cd rebar3 && ./bootstrap

# Setup Environment
ENV PATH=/buildroot/rebar3/:$PATH

# Reset working directory
WORKDIR /buildroot

# Copy our Erlang test application
COPY dockerwatch dockerwatch

WORKDIR dockerwatch

CMD rebar3 as prod release -o /artifacts
