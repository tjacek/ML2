set terminal jpeg 
set output 'chart.jpg' 
set nokey 
set yrange [0:] 
set xrange [0:16] 
set grid 
set xtics ('e2@localhost' 1, 'e2@localhost' 2, 'e2@localhost' 3, 'e2@localhost' 4, 'e2@localhost' 5, 'e2@localhost' 6, 'e2@localhost' 7, 'e2@localhost' 8, 'e2@localhost' 9, 'e2@localhost' 10, 'e2@localhost' 11, 'e2@localhost' 12, 'e2@localhost' 13, 'e2@localhost' 14, 'e2@localhost' 15)
plot '<echo 1 2001068 2 2001621 3 2000333 4 2000776 5 2001216 6 2000583 7 2000386 8 2001035 9 3010834 10 3011903 11 3009237 12 3010215 13 4016162 14 4017115 15 5020023' u 1:2 w imp lw 20, '' u 3:4 w imp lw 20, '' u 5:6 w imp lw 20, '' u 7:8 w imp lw 20, '' u 9:10 w imp lw 20, '' u 11:12 w imp lw 20, '' u 13:14 w imp lw 20, '' u 15:16 w imp lw 20, '' u 17:18 w imp lw 20, '' u 19:20 w imp lw 20, '' u 21:22 w imp lw 20, '' u 23:24 w imp lw 20, '' u 25:26 w imp lw 20, '' u 27:28 w imp lw 20, '' u 29:30 w imp lw 20
