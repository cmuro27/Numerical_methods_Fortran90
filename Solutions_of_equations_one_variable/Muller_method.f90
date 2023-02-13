program mullerreal
implicit none
integer ::  n,i
real :: p0,p1,p2,tol
real :: h1,h2,delta1,delta2,d,x,b,e,h,f,p
write(*,*)"Introducir funci¢n antes de correr el programa."
write(*,*)"Dar puntos iniciales p0, p1, p2"
read(*,*)p0,p1,p2
write(*,*)"Dar Tolerancia"
read(*,*) tol
write(*,*)"Dar n£mero de iteraciones"
read(*,*)n
!Primera parte de algoritmo
h1 = p1-p0
h2 = p2-p1
delta1=(f(p1)-f(p0))/h1
delta2 =(f(p2)-f(p1))/h2
d=(delta2-delta1)/(h2+h1)
i=0
!
do while(i<=n)
i=i+1
b=delta2+h2*d
D=sqrt((b*b)-4*(f(p2))*d)
if(abs(b-D)<abs(b+D)) then
E=b+D
else
E=b-D
end if
h=(-2.0*f(p2))/E
p=p2+h
if(abs(h)<tol)then
write(*,*)"La ra¡z es dada por ",p," en ",i,"iteraciones"
exit
end if
p0=p1
p1=p2
p2=p
h1=p1-p0
h2=p2-p1
delta1=(f(p1)-f(p0))/h1
delta2 =(f(p2)-f(p1))/h2
d=(delta2 - delta1)/(h2 + h1)
end do
!
if(i>n)then
write(*,*)" No se econtro ra¡z despu‚s de ",N ," iteraciones."
end if
!
read(*,*)
end program mullerreal
!
function f(x)
real, intent(in) :: x
f=
return
end function
