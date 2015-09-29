# gamma.plt
# Keith Herbert

set term png size 1000,600

gamma = 'my_gamma.out'

set xlabel "x"
set ylabel "Γ(x)"
set output "gamma.png"
set title "Γ(x) vs x"
set xrange [-4.5 : 4.5]
set yrange [-12 : 12]

plot gamma using 1:2 notitle