set terminal png
set output 'latency_kp_bg.png'
set xlabel "Background clients"
set ylabel "Latency in us"
set title "Avg Latency vs Num Clients"
set yrange [0:100 < * < 10000]
set style data linespoints

plot "<awk '{if($3 == 10000){print $0}}' otp-multipoll-kp-bench.res" using 1:4 title "kp multipoll, 10000 req/s",\
     "<awk '{if($3 == 10000){print $0}}' otp-poll-thread-kp-bench.res" using 1:4 title "kp poll-thread 10000 req/s",\
     "<awk '{if($3 == 50000){print $0}}' otp-multipoll-kp-bench.res" using 1:4 title "kp multipoll, 50000 req/s",\
     "<awk '{if($3 == 50000){print $0}}' otp-poll-thread-kp-bench.res" using 1:4 title "kp poll-thread 50000 req/s",\
     "<awk '{if($3 == 75000){print $0}}' otp-multipoll-kp-bench.res" using 1:4 title "kp multipoll, 75000 req/s",\
     "<awk '{if($3 == 75000){print $0}}' otp-poll-thread-kp-bench.res" using 1:4 title "kp poll-thread 75000 req/s"

# set terminal png
# set output 'latency_nkp_bg.png'
# set xlabel "Background clients"
# set ylabel "Latency in us"
# set title "Latency vs Num Clients"
# set yrange [100:* < 100000000]
# set logscale y 10
# set style data linespoints
# plot "<awk '{if($3 == 10000){print $0}}' otp-multipoll-backoff-nkp-bench.res" using 1:4 title "nkp multipoll, 10000 req/s",\
#      "<awk '{if($3 == 10000){print $0}}' otp-baseline-nkp-bench.res" using 1:4 title "nkp baseline 10000 req/s",\
#      "<awk '{if($3 == 50000){print $0}}' otp-multipoll-backoff-nkp-bench.res" using 1:4 title "nkp multipoll, 50000 req/s",\
#      "<awk '{if($3 == 50000){print $0}}' otp-baseline-nkp-bench.res" using 1:4 title "nkp baseline 50000 req/s",\
#      "<awk '{if($3 == 75000){print $0}}' otp-multipoll-backoff-nkp-bench.res" using 1:4 title "nkp multipoll, 75000 req/s",\
#      "<awk '{if($3 == 75000){print $0}}' otp-baseline-nkp-bench.res" using 1:4 title "nkp baseline 75000 req/s"

# plot "<awk -F: '{if($2 == 15 && $3 == 8){print $0}}' mydata.dat" u 1:4 w lp title 'v=15, l=8'
#set terminal png
#set output 'latency.png'
#set xlabel "Requests per second goal"
#set ylabel "Latency in us"
#set title "Latency"
#set yrange [0:100 < * < 20000]

#plot "otp-baseline-kp-bench.res" using 2:3 title "avg kp baseline",\
# "otp-multipoll-kp-bench.res" using 2:3 title "avg kp multipoll",\
# "otp-baseline-nkp-bench.res" using 2:3 title "avg nkp baseline",\
# "otp-multipoll-nkp-bench.res" using 2:3 title "avg nkp multipoll"

#plot "otp-baseline-kp-bench.res" using 2:3 title "avg kp baseline",\
# "otp-multipoll-kp-bench.res" using 2:3 title "avg kp multipoll",\
# "otp-baseline-nkp-bench.res" using 2:3 title "avg nkp baseline",\
# "otp-multipoll-nkp-bench.res" using 2:3 title "avg nkp multipoll",\
# "otp-baseline-kp-bench.res" using 2:9 title "90.0% kp baseline",\
# "otp-multipoll-kp-bench.res" using 2:9 title "90.0% kp multipoll",\
# "otp-baseline-nkp-bench.res" using 2:9 title "90.0% nkp baseline",\
# "otp-multipoll-nkp-bench.res" using 2:9 title "90.0% nkp multipoll"