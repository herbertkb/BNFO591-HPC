in_file = "thread_timing.dat"
out_file = "thread_plot.png"

set term png size 600, 1000
set output out_file


set multiplot layout 3,1 title "Thread Performance on Varying Sizes of Subsets"
unset key

set xlabel "log(threads)"
set ylabel "log(time) in ns"
unset title
set label 1 '1,000 subsets' at graph 0.8,0.9 font ',8'
plot in_file every ::0::3 using (log($2)):(log($3)) pt 7 ps 2 lc 'black'

set label 1 '1,000,000 subsets' at graph 0.75,0.9 font ',8'
plot in_file every ::4::7 using (log($2)):(log($3)) pt 7 ps 2 lc 'black' 

set label 1 '2^35 subsets' at graph 0.8,0.9 font ',8'
plot in_file every ::8::12 using (log($2)):(log($3)) pt 7 ps 2 lc 'black' 

unset multiplot


