set terminal x11
unset border
unset key
set size ratio -1
plot "/tmp/ttt.dat" i 0 using 1:2 lw 2 lt rgb "#282930" with lines, \
"" i 1 using 1:2 lw 3 lt rgb "#F00090" with lines, \
"" i 2 using 1:2 lw 3 lt rgb "#c55741" with lines, \
"" i 3 using 1:2 lw 3 lt rgb "#fab047" with lines, \
"" i 4 using 1:2 lw 7 lt rgb "#E5D11A" with lines, \
"" i 5 using 1:2 lw 1 lt rgb "#36EAF1" with lines, \

