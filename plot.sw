set term png size 800,600
set output "sw.png"
set xlabel "Skill + Wild Die"
set ylabel "Result"
set grid xtics
set grid ytics
set xtics 0,2,12 add ("Success" 4)
set datafile separator ","
set xrange[-0.5:4.5]
set yrange[0:12]
plot 'data.dat' using 0:3:2:4:xtic(1) notitle lt 6 with errorbars, 4 title "Success", 8 title "Raise"
quit
