#!/bin/bash

## Some perf commands
## perf record -p 105263 -F 999 --call-graph dwarf -- sleep 10
## perf report --kallsyms=/proc/kallsyms  --symfs /ldisk/lukas/git/docker-erlang/alpine/run/artifacts/

NUM_NETWORKS=${NUM_NETWORKS-30}
IMAGE_NAME=${IMAGE_NAME-erlang-dockerwatch}

ACTION=$1
shift

set -e

DOCKER_OPTS="--privileged --ulimit core=-1 --ulimit nofile=1000000:1000000 --volume=$PWD/core:/core --log-driver=syslog"
SERVER_DOCKER_OPTS="${DOCKER_OPTS} --cpuset-cpus=0-25 --network network1 --volume=$PWD/ssl:/etc/ssl/certs --name ${IMAGE_NAME}_server"
CLIENT_DOCKER_OPTS="${DOCKER_OPTS} --ip 172.19.1.3 --network network1 --volume=$PWD/ssl:/etc/ssl/certs --name $IMAGE_NAME-client"
WRK_DOCKER_OPTS="${DOCKER_OPTS} --cpuset-cpus=26-31"

case $ACTION in
    setup)
        sysctl -w net.ipv4.ip_local_port_range="15000 61000"
        sysctl -w net.netfilter.nf_conntrack_max=2621440
        sysctl -w net.ipv4.tcp_fin_timeout=30
        sysctl -w net.ipv4.tcp_max_syn_backlog=4096
        sysctl -w net.ipv4.tcp_syncookies=1
        sysctl -w net.core.somaxconn=1024
        ;;
    wrk)
        docker run ${WRK_DOCKER_OPTS} --network network1 --ip "172.19.1.4" --rm 1vlad/wrk2-docker $* http://172.19.1.2:8080/cnt
        # docker run --network network2 --ip "172.19.2.4" --rm 1vlad/wrk2-docker $* http://172.19.2.2:8080/cnt
        ;;
    networks)
        for i in $(seq 1 $NUM_NETWORKS); do
            docker network rm network$i
            docker network create -o com.docker.network.bridge.enable_icc=true -o com.docker.network.bridge.enable_ip_masquerade=true -o com.docker.network.bridge.host_binding_ipv4=0.0.0.0 -o com.docker.network.bridge.name=network$i -o com.docker.network.driver.mtu=1500 --driver=bridge network$i --subnet=172.19.$i.0/24
        done
        ;;
    server)
        ! docker stop -t 2 ${IMAGE_NAME}_server
        ! docker rm ${IMAGE_NAME}_server
        docker create ${SERVER_DOCKER_OPTS} $IMAGE_NAME /bin/sh -c "/dockerwatch/bin/dockerwatch foreground -setcookie foo $*"
        for i in $(seq 2 $NUM_NETWORKS); do
            docker network connect --ip "172.19.$i.2" network$i ${IMAGE_NAME}_server
        done
        exec docker start ${IMAGE_NAME}_server
        ;;
    server-console)
        ! docker stop -t 2 ${IMAGE_NAME}_server
        ! docker rm ${IMAGE_NAME}_server
        docker create ${SERVER_DOCKER_OPTS} -it $DOCKER_ARGS $IMAGE_NAME /dockerwatch/bin/dockerwatch console -setcookie foo $*
        for i in $(seq 2 $NUM_NETWORKS); do
            docker network connect --ip "172.19.$i.2" network$i ${IMAGE_NAME}_server
        done
        exec docker start -ai ${IMAGE_NAME}_server
        ;;
    server-stop)
        ! docker stop -t 2 ${IMAGE_NAME}_server
        ;;
    client-stop)
        ! docker stop -t 2 ${IMAGE_NAME}-client
        ;;
    client)
        ! docker stop -t 2 $IMAGE_NAME-client
        ! docker rm $IMAGE_NAME-client
        NUM_CLIENTS=${NUM_CLIENTS-0}
        docker create ${CLIENT_DOCKER_OPTS} $DOCKER_ARGS $IMAGE_NAME /bin/sh -c "/dockerwatch/bin/dockerwatch foreground -setcookie foo -eval 'code:load_file(dockerwatch_sup),code:load_file(bench),bench:connect($NUM_CLIENTS,20000).'"
        for i in $(seq 2 $NUM_NETWORKS); do
            docker network connect --ip "172.19.$i.3" network$i $IMAGE_NAME-client
        done
        exec docker start $IMAGE_NAME-client
        ;;
    client-console)
        ! docker stop -t 2 $IMAGE_NAME-client
        ! docker rm $IMAGE_NAME-client
        NUM_CLIENTS=${NUM_CLIENTS-0}
        docker create ${CLIENT_DOCKER_OPTS} $DOCKER_ARGS $IMAGE_NAME /dockerwatch/bin/dockerwatch console_clean -setcookie foo -eval "code:load_file(dockerwatch_sup),code:load_file(bench),bench:connect($NUM_CLIENTS,50000)."
        for i in $(seq 2 $NUM_NETWORKS); do
            docker network connect --ip "172.19.$i.3" network$i $IMAGE_NAME-client
        done
        exec docker start -ia $IMAGE_NAME-client
        ;;
    active-clients)
        SERVER_ID=$(docker ps -qf "name=${IMAGE_NAME}_server")
        NUM_CLIENTS=${NUM_CLIENTS-0}
        docker run --ip "172.19.1.5" --privileged --ulimit core=-1 --volume="$PWD/core:/core" --network network1 $IMAGE_NAME /bin/sh -c "/dockerwatch/bin/dockerwatch console_clean -noshell -setcookie foo -eval 'io:format(\"~p~n\",[rpc:call(dockerwatch@$SERVER_ID,dockerwatch_sup,wait_for_conns,[$NUM_CLIENTS])]).' -s init stop" | grep '^Current clients\|^ok'
        ;;
    clients)
        for i in $(seq 1 $NUM_NETWORKS); do
            docker stop $IMAGE_NAME-client$i
            docker rm $IMAGE_NAME-client$i
            docker create --ulimit nofile=1000000:1000000 --network network$i --ip "172.19.$i.3" -it --volume="$PWD/ssl:/etc/ssl/certs" --log-driver=syslog --name $IMAGE_NAME-client$i $IMAGE_NAME /dockerwatch/bin/dockerwatch console_clean -eval "code:load_file(dockerwatch_sup),code:load_file(bench),bench:connect(20000,250)."
            docker start $IMAGE_NAME-client$i
        done
        ;;
esac
exit 0
