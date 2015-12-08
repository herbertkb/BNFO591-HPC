in_file = "no_opt_timing.dat"
out_file = "timing_plot.png"

set term png
set output out_file

set nokey
set title "Timing"
set xlabel "log(subsets)"
set ylabel "log(time) in ns"

f(x) = m*x + b
fit f(x) in_file using 1:(log($2)) via m,b

plot in_file using 1:(log($2)) with points pt 7 ps 2 lc 'black', f(x)


