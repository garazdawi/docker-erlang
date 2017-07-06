FROM alpine:3.3

# Install additional packages
RUN apk add --no-cache autoconf && \
    apk add --no-cache alpine-sdk && \
    apk add --no-cache openssl-dev && \
    apk add --no-cache ncurses-dev && \
    apk add --no-cache git

# Build Erlang/OTP
ADD ./otp-baseline.tar.gz /buildroot/

# Build Rebar3
WORKDIR /buildroot/
RUN git clone http://github.com/erlang/rebar3

WORKDIR /buildroot/otp/

ENV MAKEFLAGS -j40

ENV ERL_TOP /buildroot/otp

#RUN ./otp_build autoconf && CFLAGS="-DERTS_MSACC_STATE_COUNTERS=1 -g -O2" ./configure --prefix=/buildroot/erlang/20.0 --disable-hipe --enable-lock-counter --with-microstate-accounting=extra
#RUN ./otp_build autoconf && CFLAGS="-DERTS_MSACC_STATE_COUNTERS=1 -g -O2" ./configure --prefix=/buildroot/erlang/20.0 --disable-hipe --enable-lock-counter
RUN ./otp_build autoconf && CFLAGS="-g -O2" ./configure --prefix=/buildroot/erlang/20.0 --disable-hipe --enable-lock-counter

# Install Erlang/OTP
RUN mkdir -p /buildroot/erlang/20.0 && make

RUN make install

ADD ./otp.tar.gz /buildroot/otp2/

RUN cp -r ../otp2/otp/erts/* erts

RUN (cd erts/ && ./config.status) && make && make install

ENV PATH=/buildroot/erlang/20.0/bin:$PATH

WORKDIR /buildroot/rebar3
RUN ./bootstrap

# Setup Environment
ENV PATH=/buildroot/rebar3/:$PATH

# Reset working directory
WORKDIR /buildroot

# Copy our Erlang test application
COPY dockerwatch dockerwatch

WORKDIR dockerwatch

CMD rebar3 as prod release -o /artifacts
