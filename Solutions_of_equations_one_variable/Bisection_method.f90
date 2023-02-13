program bisec
implicit none
integer :: i
real :: a,b,fa,fb,x,fun,p,fp,tol
write(*,*)"Antes de compilar ingresar funcion f(x)"
write(*,*)"Definir intervalo a,b"
read(*,*)a,b
write(*,*)"Introducir tolerancia de aproximaciones"
read(*,*)tol
i=0
x=a
fa=fun(a)
x=b
fb=fun(b)
write(*,*)"Los valores de f(a) y de f(b) son"
print*, fa,fb
p=(a+b)/2
x=p
fp=fun(x)
!
condd: if((fa*fb)>0)then
write(*,*)"No existe ra¡z real en dicho intervalo"
stop "Verifica intervalo"
end if condd
!
!
write(*,*)"Sea p el punto medio en las bisecciones."
write(*,*)"Los puntos p y f(p) son: "
write(*,3)p,fp
3 format(f12.8,"  ",f12.8)
!
cond1: if(abs(fp)<tol.or.fp==0)then
write(*,6)p,fp
6 format("El valor de p es ",f12.8," ya que f(p) vale",f12.8)
stop "acab¢"
else
i=0
do while (abs(fp)>tol.or.fp/=0.or.i<=30)
i=i+1
if((fp*fa)<0)then
a=a
b=p
p=(a+b)/2
x=p
fp=fun(x)
i=i+1
write(*,4)i,p,fp
4 format(I2,f10.6,"  ",f10.6)
end if
if((fp*fb)<0)then
b=b
a=p
p=(a+b)/2
x=p
fp=fun(x)
i=i+1
write(*,5)i,p,fp
5 format(I2,f10.6,"  ",f10.6)
end if
cond2: if(abs(fp)<tol.or.fp==0)then
exit
end if cond2
end do
end if cond1
!
read(*,*)
end program bisec

function fun (x)
real, intent(in) :: x
fun=1+(0.1548459/(1.18501*(2.71828**(-10*x))-1.3434470*(2.71828**(-20*x))))-1.185011*(1+(0.1548459/1.18501*(2.71828**(-10*x))))
return
end function fun
