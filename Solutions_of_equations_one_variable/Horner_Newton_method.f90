program HornNewton
implicit none
real(kind=8), allocatable, dimension(:)	::	a
real(kind=8) :: y,z,x0,x,tol,p,px,fun,fp
integer	:: i, j, n,k
k=0
!
write(*,*)"Ingresa el grado del polinomio: "
read(*,*) n
allocate(a(0:n))

write(*,*)"Introducir Tolerancia"
read(*,*)tol
write(*,*)"Escribe los coeficientes, desde el de mayor grado."
DO i=n,0,-1
read(*,*)A(i)
END DO
write(*,*)"Tus coeficientes son"
write(*,*) A
write(*,*)"Ingresa tu xo: "
read(*,*) x0
y=a(n)
z=a(n)
write(*,*)"Los valores de y,z, la aproximaci¢n p, y f(p) son:"
do while(abs(y)>tol.or.k<=30.or.y/=0)
y=a(n)
z=a(n)
k=k+1
do j=n-1,1,-1
y=x0*y+a(j)
z=x0*z+y
end do
y=x0*y+a(0)
!Hacemos aproximaci¢n con Newton
p=x0-(y/z)
x=p
write(*,7)y,z,p,y
7 format(" ",f15.8," ",f15.8," ", f15.8," ",f15.8)
x0=p
if(abs(y)<tol.or.y==0.or.k>30)then
exit
end if
end do
read(*,*)
end program  HornNewton
!
!

 




