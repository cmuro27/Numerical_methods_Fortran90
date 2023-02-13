program RungeKuttaSist
implicit none
real, allocatable, dimension(:,:) :: k
integer :: i,n
real :: h,b,a,t,f1,f2,u2,u1,w2,w1,ni
write(*,*)"Este sistema es solo para sistemas de dos ecuaciones."
write(*,*)"Debe introducir las 2 ecuaciones antes de compilar"
allocate(k(4,2))
write(*,*)"Ingresar intervalo a,b"
read(*,*)a,b
write(*,*)"Ingrese el n£mero N, del tama¤o de paso h=(b-a)/N."
read(*,*)N
ni=n
h=(b-a)/ni
t=a
write(*,*)"Ingrese las dos condiciones iniciales"
read(*,*)w1,w2
write(*,*)"Las aproximaciones por el m‚todo de Runge-Kutta para sistemas de ecuaciones son:"
write(*,*)t,w1,w2
do i=1,n
k(1,1)=h*f1(t,w1,w2)
k(1,2)=h*f2(t,w1,w2)
k(2,1)=h*f1(t+0.5*h,w1+0.5*k(1,1),w2+0.5*k(1,2))
k(2,2)=h*f2(t+0.5*h,w1+0.5*k(1,1),w2+0.5*k(1,2))
k(3,1)=h*f1(t+0.5*h,w1+0.5*k(2,1),w2+0.5*k(2,2))
k(3,2)=h*f2(t+0.5*h,w1+0.5*k(2,1),w2+0.5*k(2,2))
k(4,1)=h*f1(t+h,w1+k(3,1),w2+k(3,2))
k(4,2)=h*f2(t+h,w1+k(3,1),w2+k(3,2))
w1=w1+((k(1,1)+(2*k(2,1))+(2*k(3,1))+k(4,1))/6)
w2=w2+((k(1,2)+(2*k(2,2))+(2*k(3,2))+k(4,2))/6)
t=a+i*h
write(*,*)t,w1,w2
end do
read(*,*)
end program RungeKuttaSist


function f1 (t,u1,u2)
real, intent(in) :: t,u1,u2
f1=u2
return
end function f1
!
function f2 (t,u1,u2)
real, intent(in) :: t,u1,u2
f2=t*((2.71828182846)**(t))+(2*u2)-u1-t
return
end function f2

