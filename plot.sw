set term png size 800,600
set output "sw.png"
set xlabel "Skill + Wild Die"
set ylabel "Result"
set grid xtics
set grid ytics
set mxtics 4
set datafile separator ","
set xrange[-1:5]
set yrange[0:12]
unset key
plot 'data.dat' using 0:3:2:4:xtic(1) title "Savage Worlds" with errorbars
quit
