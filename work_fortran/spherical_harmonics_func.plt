reset

set parametric
set zeroaxis
unset xtics
unset ytics
unset ztics
set angle degree
#set urange[0:360]
set vrange[0:360]
set isosample 18,18
set ticslevel 0
set size 0.65,1.0
#set pm3d
set iso 60
#unset surf
a=1.0/(4*pi)
gxy(u,v)=cos(v)**2*sin(2*u)
gx2y2(u,v)=cos(v)**2*cos(2*u)
#clr(u)=(cos(2*u)>0) ? 256**2*255:255
#clr=256**3-1
fx(u,v)=cos(u)*cos(v)
fy(u,v)=sin(u)*cos(v)
fz(v)=sin(v)
splot [u=0:90] a*fx(u,v)*gxy(u,v),a*fy(u,v)*gxy(u,v),a*fz(v)*gxy(u,v) lc rgb "#0000ff",a*fx(u+90,v)*gxy(u+90,v),a*fy(u+90,v)*gxy(u+90,v),a*fz(v)*gxy(u+90,v) lc rgb "#ff0000"
