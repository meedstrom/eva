#!/usr/bin/env gnuplot
set term dumb size 60,20 nofeed
set tics out nomirror
set xdata time
set timefmt "%Y-%m-%d"
set format x "%b %d"
set yrange [0:5]
set xrange ["2021-01-01":]
plot "/tmp/secretary/mood.dat" using 1:2 notitle

# Use the mood_desc column as labels (not suitable for ascii).
# plot "/tmp/secretary/mood.dat" using 1:2 notitle,\
#      "" using 1:2:3 notitle with labels
