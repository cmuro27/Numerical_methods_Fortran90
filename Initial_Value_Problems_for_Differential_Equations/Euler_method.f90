program euler7
implicit none
integer :: i
real :: y,h,a,b,t,teta,f,w,N
write(*,*)"Antes de compilar, ingresar la funci¢n f(t,y)"
write(*,*)"Ingresar intervalo a,b"
read(*,*)a,b
write(*,*)"Ingrese condici¢n inicial y(a)=teta, es decir, ingrese teta."
read(*,*)teta
write(*,*)"Ingrese el n£mero N, del tama¤o de paso h=(b-a)/N."
read(*,*)N
h=(b-a)/N
t=a
w=teta
print*,h
write(*,*)"Los puntos red del m‚todo de Euler son: "
write(*,*)t,w
do i=1,N
w=w+h*(f(t,w))
t=a+i*h
write(*,*)t,w
end do
read(*,*)
end program euler7
!
!
!
 function f (t,y)
real, intent(in) :: t,y
f=y-(t**2)+1
return
end function f
