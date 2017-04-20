#!/bin/bash

set -e

LOGFILE=../bench.log
! rm $LOGFILE

KERNEL_POLL=${KERNEL_POLL-true false}
DOCKER_ARGS=--cpuset-cpus="0-24"

WRK_CLIENTS=${WRK_CLIENTS-1000}
WRK_RATES=${WRK_RATES-10000 50000 75000}
#WRK_RATES=${WRK_RATES-1000 10000 25000 50000 60000 70000 80000 90000 100000 110000 120000 130000}
BACKGROUND_CLIENTS=${BACKGROUND_CLIENTS-0 10000 25000 50000 75000 100000 200000 300000 400000}

for IMAGE in $*; do
    cp $IMAGE.tar.gz otp.tar.gz
    export IMAGE_NAME=$IMAGE-bench
    echo "Building $IMAGE... see $LOGFILE for details"
    ./create-image $IMAGE_NAME >> $LOGFILE

    for KP in $KERNEL_POLL; do
        if [ $KP = true ]; then
            RES_FILE=$IMAGE-kp-bench.res
        else
            RES_FILE=$IMAGE-nkp-bench.res
        fi
        ./start.sh server +K $KP >> $LOGFILE
        sleep 1

        ! rm $RES_FILE >> $LOGFILE
        TEMP=$(mktemp)

        for BACKGROUND in $BACKGROUND_CLIENTS; do
            echo "Running $BACKGROUND background connections"
            NUM_CLIENTS=$BACKGROUND ./start.sh client >> $LOGFILE
            CURR_CLIENTS=0

            while [ $CURR_CLIENTS -lt $(( BACKGROUND*95/100 )) ]; do
                sleep 5
                CURR_CLIENTS=`./start.sh active-clients`
                echo "Current clients $CURR_CLIENTS"
            done

            for CLIENTS in $WRK_CLIENTS; do
                for RATE in $WRK_RATES; do
                    echo "Running -c$CLIENTS -R$RATE"
                    echo -n "$BACKGROUND $CLIENTS $RATE " >> $RES_FILE
                    docker run --cpuset-cpus="25-31" --network network1 --ip "172.19.1.4" --rm 1vlad/wrk2-docker -t6 -c$CLIENTS -d15s -R$RATE -L http://172.19.1.2:8080/cnt > $TEMP
                    grep "    Latency" $TEMP | awk '{printf $2 " " $3 " " $4 " "}' | awk -f ts-convert.awk >> $RES_FILE
                    grep "Requests/sec" $TEMP | awk '{printf $2}' >> $RES_FILE
                    grep "^[ 0-9.]\+%" $TEMP | awk '{ printf $2 " " }' | awk -f ts-convert.awk >> $RES_FILE
                    echo "" >> $RES_FILE
                    sleep 1
                done
            done
            ./start.sh client-stop >> $LOGFILE
        done
        rm $TEMP
        ./start.sh server-stop >> $LOGFILE
    done
done

docker run --rm --volume $PWD:/tmp -w /tmp pavlov99/gnuplot gnuplot graph.gp
