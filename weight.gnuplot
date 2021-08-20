#!/usr/bin/env gnuplot
set term dumb size 70,20 nofeed
set tics out nomirror
set xdata time
set timefmt "%Y-%m-%d"
set format x "%b"
set xrange ["2020-07-01":]
plot "/tmp/eva/weight.dat" using 1:2 notitle
