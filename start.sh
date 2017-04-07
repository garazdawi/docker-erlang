#!/bin/bash

## Some perf commands
## perf record -p 105263 -F 999 --call-graph dwarf -- sleep 10
## perf report --kallsyms=/proc/kallsyms  --symfs /ldisk/lukas/git/docker-erlang/alpine/run/artifacts/

NUM_NETWORKS=30

case $1 in
    setup)
        sysctl net.ipv4.ip_local_port_range="15000 61000"
        sysctl net.netfilter.nf_conntrack_max=2621440
        sysctl net.ipv4.tcp_fin_timeout=30
        sysctl net.ipv4.tcp_max_syn_backlog=4096
        sysctl net.ipv4.tcp_syncookies=1
        sysctl net.core.somaxconn=1024
        ;;
    wrk)
        docker run --network network1 --ip "172.19.1.4" --rm 1vlad/wrk2-docker -t1 -c4 -d30s -R100 http://172.19.0.2:8080/cnt
        ;;
    networks)

        for i in $(seq 1 $NUM_NETWORKS); do
            docker network rm network$i
            docker network create -o com.docker.network.bridge.enable_icc=true -o com.docker.network.bridge.enable_ip_masquerade=true -o com.docker.network.bridge.host_binding_ipv4=0.0.0.0 -o com.docker.network.bridge.name=network$i -o com.docker.network.driver.mtu=1500 --driver=bridge network$i --subnet=172.19.$i.0/24
        done
        ;;
    server)
        docker stop erlang-dockerwatch
        docker rm erlang-dockerwatch
        docker create --ulimit core=-1 --ulimit nofile=1000000:1000000 --network network1 -it --volume="$PWD/ssl:/etc/ssl/certs" --volume="$PWD/core:/core" --cpuset-cpus="0-15" --log-driver=syslog --name erlang-dockerwatch erlang-dockerwatch
        for i in $(seq 2 $NUM_NETWORKS); do
            docker network connect --ip "172.19.$i.2" network$i erlang-dockerwatch
        done
        exec docker start -ia erlang-dockerwatch
        ;;
    client)
        docker stop erlang-dockerwatch-client
        docker rm erlang-dockerwatch-client
        docker create --ulimit nofile=1000000:1000000 --network network1 -it --volume="$PWD/ssl:/etc/ssl/certs" --log-driver=syslog --cpuset-cpus="16-31" --name erlang-dockerwatch-client erlang-dockerwatch /dockerwatch/bin/dockerwatch console_clean -eval "code:load_file(dockerwatch_sup),code:load_file(bench)."
        for i in $(seq 2 $NUM_NETWORKS); do
            docker network connect --ip "172.19.$i.3" network$i erlang-dockerwatch-client
        done
        exec docker start -ia erlang-dockerwatch-client
        ;;
    clients)
        for i in $(seq 1 $NUM_NETWORKS); do
            docker stop erlang-dockerwatch-client$i
            docker rm erlang-dockerwatch-client$i
            docker create --ulimit nofile=1000000:1000000 --network network$i --ip "172.19.$i.3" -it --volume="$PWD/ssl:/etc/ssl/certs" --log-driver=syslog --name erlang-dockerwatch-client$i erlang-dockerwatch /dockerwatch/bin/dockerwatch console_clean -eval "code:load_file(dockerwatch_sup),code:load_file(bench),bench:connect(20000,250)."
            docker start erlang-dockerwatch-client$i
        done
        ;;
esac
