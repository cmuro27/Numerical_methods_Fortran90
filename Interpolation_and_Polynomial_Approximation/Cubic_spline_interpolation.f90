program splincubinatu
implicit none
real, allocatable, dimension(:) :: x,a,h,b,d
real, allocatable, dimension(:) :: t,l,u,z,c
integer :: n,i,j
write(*,*)"Este programa te da los coeficientes de los splines c£bicos naturales"
write(*,*)"en cada intervalo. Donde Sj(x)=aj+bj(x-xj)+cj(x-xj)^2+dj(x-xj)^3 "
write(*,*)"¨Cu ntos nodos son?."
read(*,*)n
n=n-1
allocate(x(0:n),a(0:n),h(0:n),t(0:n),l(0:n),u(0:n),z(0:n))
allocate(b(0:n),c(0:n),d(0:n))
write(*,*)"Introducir los valores de los nodos separados por coma."
read(*,*)x
write(*,*)"Introducir valores de la funci¢n por cada nodo separados por coma."
read(*,*)a
do i=0,n-1
h(i)=x(i+1)-x(i)
end do
do i=1,n-1
t(i)=(3/h(i))*(a(i+1)-a(i))-(3/h(i-1))*(a(i)-a(i-1))
end do
l(0)=1
u(0)=0
z(0)=0
do i=1,n-1
l(i)=2*(x(i+1)-x(i-1))-h(i-1)*u(i-1)
u(i)=h(i)/l(i)
z(i)=(t(i)-h(i-1)*z(i-1))/l(i)
end do
l(n)=1
z(n)=0
c(n)=0
do j=n-1,0,-1
c(j)=z(j)-u(j)*c(j+1)
b(j)=((a(j+1)-a(j))/h(j))-h(j)*(c(j+1)+2*c(j))/3
d(j)=(c(j+1)-c(j))/(3*h(j))
end do
write(*,*)"Los coeficientes de cada polinomio c£bico S(x), en"
write(*,*)"orden de los intervalos por filas y en orden "
write(*,*)"de menor a mayor grado por columnas, son:"
do j=0,n-1
write(*,2)a(j),b(j),c(j),d(j)
2 format(f12.8," ",f12.8," ",f12.8," ",f12.8)
end do
!
read(*,*)
end program splincubinatu
