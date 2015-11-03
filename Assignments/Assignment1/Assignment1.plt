# Assignment1.plt
# Keith Herbert

set term png size 3000,2000

Atanasov_table1 = 'Atanasov_table1_clean.txt'

set xlabel "Mass (kg)"
set ylabel "Lifespan (yrs"


set output "plot1.png"
set title "Mass vs. Lifespan"
plot Atanasov_table1 using 2:3 notitle

set output "plot2.png"
set title "Mass vs. Lifespan labeled by Species"
plot Atanasov_table1 using 2:3 notitle, '' using 2:3:1 with labels offset 0.5,1 notitle

set output "plot3.png"
set title "Mass vs. Lifespan Spline Curve"
plot Atanasov_table1 using 2:3:(1.0) smooth acsplines notitle, '' using 2:3:1 with labels offset 0.5,1 notitle  

set output "plot4.png"
set title "ln(Mass) vs ln(Lifespan)"
plot Atanasov_table1 using ( log($2) ):( log($3) ) notitle, '' using ( log($2) ):( log($3) ):1 with labels offset 0.5,0.5 notitle

set output "plot5.png"
set title "ln(Mass) vs ln(Lifespan) with linear regression"
plot Atanasov_table1 using ( log($2) ):( log($3) ) notitle, '' using ( log($2) ):( log($3) ):1 with labels offset 0.5,0.5 notitle, '' using ( log($2) ):( log($3) ):(1/1000.) smooth acsplines 

# There is a clear correlation between a bird's mass and its lifespan. The larger a bird is, the longer it lives. This is most visible from the extremes at either end of the spectrum: Strutio camelus weighs 100kg and lives 45 years compared to Selasphorus platycercus which weighs a mere three grams and has a life span of only four years. The trend continues for other birds in the table but is hard to observe by plotting mass directly against lifespan. Most of the species weigh less than a kilogram and cluster too tightly to the left end of the plot for a trend to be observable. Connecting the points with a spline curve seems to imply a longer lifespan with greater mass but it declines after 20kg. However, a plot of the logs of the mass and lifespan shows a more direct linear relationship. The previous cluster below 1 kg on the previous graph is spread out across a much wider range of the graph. This makes the trend much more visible. In addition, the very heavy birds which were outliers on the previous graphs now show as being within an acceptable distance from the linear regression line. I plotted this line by setting the parameter for the spline feature to near 0, a trick I pulled from the book GNUplot in Action.   