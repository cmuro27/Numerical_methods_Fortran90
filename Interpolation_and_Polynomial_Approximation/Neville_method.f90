program neville
!Este programa da la tabla con la iteraci¢n
!de polinomios de Lagrange con el m‚todo de Newivlle
implicit none
real, allocatable, dimension(:,:) :: q
real, allocatable, dimension(:) :: x
real :: x0
integer :: n,i,j,k,m,l
write(*,*)"¨Cu ntos nodos son?."
read(*,*)n
n=n-1
allocate(q(0:n,0:n))
allocate(x(0:n))
write(*,*)"Introducir el valor para el cual se quiere aproximar el polinomio."
read(*,*)x0
write(*,*)"Introducir valores de los nodos separados por coma."
read(*,*)x
write(*,*)"Introducir los correspondientes valores de los nodos separados por enter."
do m=0,n
read(*,*)q(m,0)
end do
do i=1,n
do j=1,i
q(i,j)=(((x0-x(i-j))*q(i,j-1))-(x0-x(i))*(q(i-1,j-1)))/(x(i)-x(i-j))
end do
end do
write(*,*)"Los nodos son:"
write(*,*)x
write(*,*)"La tabla de Neville es:"
do k=0,n
write(*,*)q(k,:)
end do
!
read(*,*)
end program neville



