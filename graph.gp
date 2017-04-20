set terminal png
set output 'requests.png'
set xlabel "Requests per second goal"
set ylabel "Requests per second achieved"
set title "Requests per second"
set style data linespoints
plot "otp-baseline-kp-bench.res" using 2:6 title "kp baseline",\
 "otp-multipoll-kp-bench.res" using 2:6 title "kp multiplot",\
 "otp-baseline-nkp-bench.res" using 2:6 title "nkp baseline",\
 "otp-multipoll-nkp-bench.res" using 2:6 title "nkp multiplot"

set terminal png
set output 'latency.png'
set xlabel "Requests per second goal"
set ylabel "Latency in us"
set title "Latency"
set yrange [0:100 < * < 20000]

plot "otp-baseline-kp-bench.res" using 2:3 title "avg kp baseline",\
 "otp-multipoll-kp-bench.res" using 2:3 title "avg kp multiplot",\
 "otp-baseline-nkp-bench.res" using 2:3 title "avg nkp baseline",\
 "otp-multipoll-nkp-bench.res" using 2:3 title "avg nkp multiplot"

#plot "otp-baseline-kp-bench.res" using 2:3 title "avg kp baseline",\
# "otp-multipoll-kp-bench.res" using 2:3 title "avg kp multiplot",\
# "otp-baseline-nkp-bench.res" using 2:3 title "avg nkp baseline",\
# "otp-multipoll-nkp-bench.res" using 2:3 title "avg nkp multiplot",\
# "otp-baseline-kp-bench.res" using 2:9 title "90.0% kp baseline",\
# "otp-multipoll-kp-bench.res" using 2:9 title "90.0% kp multiplot",\
# "otp-baseline-nkp-bench.res" using 2:9 title "90.0% nkp baseline",\
# "otp-multipoll-nkp-bench.res" using 2:9 title "90.0% nkp multiplot"