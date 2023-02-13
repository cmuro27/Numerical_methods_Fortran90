program intgrales
implicit none
real, allocatable, dimension (:) :: x,fx
integer :: n
real :: f,xo,a,b,hx(i)
integer :: i,log
write(*,*)"Ingresar funci¢n antes de compilar el programa."
write(*,*)"Las f¢rmulas cerradas, y abiertas del programa solo son"
write(*,*)"precisas en intervalos peque¤os."
write(*,*)"Desea utilizar las f¢rmulas cerradas o abiertas."
write(*,*)"Si desea las f¢rmulas cerradas de regla del trapecio o la regla "
write(*,*)"del m‚todo de Simpson presione 1."
write(*,*)"Si desea las f¢rmulas abiertas de Newton Cotes presiones 2 "
read(*,*)log
!
!
if(log==1)then
write(*,*)"Introducir intervalo a,b."
read(*,*)a,b
write(*,*)"¨Cu ntos nodos son?"
read(*,*)n
if(n==2)then
n=n-1
allocate(x(0:n),fx(0:n))
h=(b-a)/n
do i=0,n-1
x(0)=a
x(i+1)=x(i)+i*h
fx(i)=f(x(i))
end do
xi=(h/2)*(sum(fx))
write(*,*)"El valor de la integral por la regla del trapecio es: "
write(*,*)xi
end if
if(n==3)then
n=n-1
allocate(x(0:n),fx(0:n))
h=(b-a)/n
do i=0,n-1
x(0)=a
x(i+1)=x(i)+i*h
fx(i)=f(x(i))
end do
xi=(h/3)*(fx(0)+4*fx(1)+fx(2))
write(*,*)"El valor de la integral por la regla de Simpson es: "
write(*,*)xi
end if
if(n==4.or.n==5)then
n=n-1
allocate(x(0:n),fx(0:n))
h=(b-a)/n
do i=0,n-1
x(0)=a
x(i+1)=x(i)+i*h
fx(i)=f(x(i))
end do
if(n==4)then
xi=(3*(h/8)))*(fx(0)+3*(fx(1)+fx(2))+fx(3))
write(*,*)"El valor de la integral por la regla de tres octavos de Simpson es: "
write(*,*)xi
end if
if(n==5)then
xi=(2*(h/45))*(7*fx(0)+32*fx(1)+12*fx(2)+32*fx(3)+7*fx(4))
write(*,*)"El valor de la integral por la regla de tres octavos de Simpson es: "
write(*,*)xi
end if
end if








end program intgrales
!
function  f (x)
real, intent(in) :: x
f=
return
end function f
