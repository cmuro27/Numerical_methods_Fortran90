program intgralscompstas
implicit none
integer :: lg,i
real :: h,f,x,xio,xii,xis,xi,sumf,n,a,b
write(*,*)"Ingresar funci¢n antes de compilar el programa."
write(*,*)"En el programa se deben ingresar el n£mero de subintervalos"
write(*,*)"en los que se divide el intervalo, debe ser n£mero par. El tama¤o"
write(*,*)"de los intervalos debe ser peque¤o."
write(*,*)"¨Qu‚ m‚todo desea, el m‚todo de Simpson o la regla del trapecio?."
write(*,*)"Si quiere m‚todo del trapecio compuesto presione a continuaci¢n 1."
write(*,*)"Si quiere la regla de Simpson compuesta presione 2."
read(*,*)lg
!
!
if(lg==1)then
write(*,*)"Ingresar intervalo de integraci¢n"
read(*,*)a,b
write(*,*)"¨Cu l es el n£mero de subintervalos (par)?"
read(*,*)n
h=(b-a)/n
sumf=0
do i=1,n-1
x=a+i*h
sumf=sumf+f(x)
end do
xi=(h/2)*(f(a)+f(b)+2*sumf)
write(*,*)"La aproximaci¢n al valor de la integral por ‚l m‚todo"
write(*,*)"del trapecio compuesto es: "
write(*,*)xi
end if

!
if(lg==2)then
write(*,*)"Ingresar intervalo de integraci¢n"
read(*,*)a,b
write(*,*)"¨Cu l es el n£mero de subintervalos (par)?"
read(*,*)n
h=(b-a)/n
xio=f(a)+f(b)
xio=0
xis=0
do i=1,n-1
x=a+i*h
if(mod(i,2)==0)then
xis=xis+f(x)
else
xii=xii+f(x)
end if
xi=h*(xio+2*xis+4*xii)/3
end do
write(*,*)"La aproximaci¢n al valor de la integral por ‚l"
write(*,*)"m‚todo de Simpson compuesto es: "
write(*,*)xi
end if
!
!
read(*,*)
end program intgralscompstas
!
!
function  f (x)
real, intent(in) :: x
f=(2.71828)**(-x**2)
return
end function f


