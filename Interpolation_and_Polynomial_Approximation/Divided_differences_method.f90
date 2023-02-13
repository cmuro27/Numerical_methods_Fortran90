program difediv
!Este programa obtiene los coeficientes del polinomio interpolante
!De la forma Pn(x)=a0+a1(x-x0)+a2(x-x1)(x-x0)+...+an(x-x0)(x-x1)...(x-xn)
implicit none
real, allocatable, dimension(:,:) :: F
real, allocatable, dimension(:) :: x
integer :: n,m,i,j,k
write(*,*)"¨Cu ntos nodos son?."
read(*,*)n
n=n-1
allocate(F(0:n,0:n))
allocate(x(0:n))
write(*,*)"Introducir valores de los nodos separados por coma."
read(*,*)x
write(*,*)"Introducir los correspondientes valores de los nodos separados por enter."
do m=0,n
read(*,*)F(m,0)
end do
do i=1,n
do j=1,i
F(i,j)=(F(i,j-1)-F(i-1,j-1))/(x(i)-x(i-j))
end do
end do
write(*,*)"Los nodos son:"
write(*,*)x
write(*,*)"La tabla de las diferencias divididas, donde los primeros elementos"
write(*,*)"de cada columna son los coeficientee del polinomio interpolante de Newton, es:"
do k=0,n
write(*,*)F(k,:)
end do
!
read(*,*)
end program difediv

