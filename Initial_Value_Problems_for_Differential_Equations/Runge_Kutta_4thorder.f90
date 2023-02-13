program rungekutta4or
implicit none
integer :: i,n
real :: x,h,a,b,t,teta,f,w,k1,k2,k3,k4,ni,r,g,torr
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
write(*,*)"Las aproximaciones por el m‚todo de Runge-Kutta de orden cuatro son:"
write(*,*)t,w
do i=1,N
k1=h*(f(t,w))
k2=h*f(t+0.5*h,w+0.5*k1)
k3=h*f(t+0.5*h,w+0.5*k2)
k4=h*f(t+h,w+k3)
w=w+((k1+(2*k2)+(2*k3)+k4)/6)
t=a+i*h
write(*,*)t,w
end do
read(*,*)
end program rungekutta4or
!
!
!
!
function f (t,y)
real, intent(in) :: t,y
f=-0.6*(0.01)*sqrt(2*32.1*y)/(y**2)
return
end function f
