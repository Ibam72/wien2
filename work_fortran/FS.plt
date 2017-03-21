reset
unset key
set size square
set xtics("0" 0)
set ytics("0" 0)
set xrange [-3.1416:3.1416]
set yrange [-3.1416:3.1416]
set border lw 1
set palette defined (0 '#ff0000',1 '#00ff00')

plot 'FS_rfill_1.0.dat' w l palette lw 3

reset
