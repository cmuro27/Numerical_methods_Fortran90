program deripuntos
implicit none
real, allocatable, dimension(:) :: x,f,fpri
real :: h
integer :: n,i
write(*,*)"¨Cu ntos nodos son, 3 ¢ 5 ?."
write(*,*)"Recuerda que para que las f¢rmulas sirven, los nodos deben"
write(*,*)"estar igualmente espaciados. Ordenar de menor a mayor"
read(*,*)n
n=n-1
allocate(x(0:n),f(0:n),fpri(0:n))
write(*,*)"Introducir los valores de los nodos x separados por coma."
read(*,*)x
write(*,*)"Introducir los valores de f(x) separados por coma."
read(*,*)f
!
!
if(n==2)then
write(*,*)"Los valores de x, f(x) y f`(x) son: "
do i=0,n
if(i==0)then
h=x(1)-x(0)
fpri(i)=(1/(2*h))*((-3)*f(i)+4*f(i+1)-f(i+2))
write(*,*)x(i)," ",f(i)," ",fpri(i)
end if
if(i==2)then
h=x(0)-x(1)
fpri(i)=(1/(2*h))*((-3)*f(i)+4*f(i-1)-f(i-2))
write(*,*)x(i)," ",f(i)," ",fpri(i)
end if
if(i==1)then
h=x(1)-x(0)
fpri(i)=(1/(2*h))*(f(i+1)-f(i-1))
write(*,*)x(i)," ",f(i)," ",fpri(i)
end if
end do
end if
!
!
if(n==4)then
write(*,*)"Usando las  f¢rmulas de los cinco puntos tenemos que"
write(*,*)"Los valores de x, f(x) y f`(x) son:"
do i=0,n
if(i==0)then
h=x(1)-x(0)
fpri(i)=(1/(2*h))*((-3)*f(i)+4*f(i+1)-f(i+2))
write(*,*)x(i)," ",f(i)," ",fpri(i)
end if
if(i==4)then
h=x(0)-x(1)
fpri(i)=(1/(2*h))*((-3)*f(i)+4*f(i-1)-f(i-2))
write(*,*)x(i)," ",f(i)," ",fpri(i)
end if
if(i==1)then
h=x(1)-x(0)
fpri(i)=(1/(2*h))*((-3)*f(i)+4*f(i+1)-f(i+2))
write(*,*)x(i)," ",f(i)," ",fpri(i)
end if
if(i==3)then
h=x(0)-x(1)
fpri(i)=(1/(2*h))*((-3)*f(i)+4*f(i-1)-f(i-2))
write(*,*)x(i)," ",f(i)," ",fpri(i)
end if
if(i==2)then
h=x(1)-x(0)
fpri(i)=(1/(2*h))*(f(i+1)-f(i-1))
print*, "Con punto medio: ",x(i)," ",f(i)," ",fpri(i)
fpri(i)=(1/(2*h))*((-3)*f(i)+4*f(i+1)-f(i+2))
print*, "Con diferencias hacia adelante: ",x(i)," ",f(i)," ",fpri(i)
h=x(0)-x(1)
fpri(i)=(1/(2*h))*((-3)*f(i)+4*f(i-1)-f(i-2))
print*, "Con diferencias hacia atr s: ",x(i)," ",f(i)," ",fpri(i)
end if
end do
write(*,*)"Ahora, usando las f¢rmulas de los cinco puntos:"
do i=0,n
if(i==0)then
h=x(1)-x(0)
fpri(i)=(1/(12*h))*(-25*f(i)+48*f(i+1)-36*f(i+2)+16*f(i+3)-3*f(i+4))
write(*,*)x(i)," ",f(i)," ",fpri(i)
end if
if(i==4)then
h=x(0)-x(1)
fpri(i)=(1/(12*h))*(-25*f(i)+48*f(i-1)-36*f(i-2)+16*f(i-3)-3*f(i-4))
write(*,*)x(i)," ",f(i)," ",fpri(i)
end if
if(i==2)then
h=x(1)-x(0)
fpri(i)=(1/(12*h))*(f(i-2)-8*f(i-1)+8*f(i+1)-f(i+2))
write(*,*)x(i)," ",f(i)," ",fpri(i)
end if
end do
end if
read(*,*)
end program deripuntos
