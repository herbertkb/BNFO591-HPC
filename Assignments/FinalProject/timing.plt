in_file = "no_opt_timing.dat"
out_file = "timing_plot.png"

set term png
set output out_file


set title "Timing"
set xlabel "N"
set ylabel "log(time) in ns"

plot in_file using 1:(log($2)) with points pt 7 ps 2 lc 'black'