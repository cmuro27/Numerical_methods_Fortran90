program hermite
!El polinomio oscilante es aquel que aproxima a f
!y sus derivadas. Aqui obtenemos coeficientes de dicho polinomio
implicit none
real, allocatable, dimension(:) :: x,f,df,z
real, allocatable, dimension(:,:) :: q
integer :: n,i,j,k,m
write(*,*)"¨Cu ntos nodos son?."
read(*,*)n
n=n-1
allocate(x(0:n),f(0:n),df(0:n),z(0:2*n),q(0:2*n+1,0:2*n+1))
write(*,*)"Introducir valores de los nodos separados por coma."
read(*,*)x
write(*,*)"Introducir valores de la funci¢n por cada nodo separados por coma."
read(*,*)f
write(*,*)"Introducir valores de las derivadas en los nodos separados por coma."
read(*,*)df
do i=0,n
z(2*i)=x(i)
z(2*i+1)=x(i)
Q(2*i,0)=f(i)
q(2*i+1,0)=f(i)
q(2*i+1,1)=df(i)
if(i/=0)then
q(2*i,1)=(q(2*i,0)-q(2*i-1,0))/(z(2*i)-z(2*i-1))
end if
end do
do k=2,2*n+1
do j=2,k
q(k,j)=(q(k,j-1)-q(k-1,j-1))/(z(k)-z(k-j))
end do
end do
write(*,*)"Los nodos son:"
write(*,*)x
write(*,*)"La tabla de Hermite, donde los primeros elementos de cada"
write(*,*)"columna son los coeficientes del polinomio interpolante, es:"
do m=0,2*n+1
write(*,*)q(m,:)
end do
!
read(*,*)
end program hermite


