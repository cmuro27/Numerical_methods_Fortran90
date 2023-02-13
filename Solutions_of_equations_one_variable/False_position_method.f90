program regulafalsi
!M‚todo de la posici¢n falsa
!que consiste simplemente en ir encerrando en la ra¡z con
!las secantes
implicit none
real :: a,b,fa,fb,x,fun,p,fp,tol,p0,p1,fp0,fp1
integer :: i
write(*,*)"Definir intervalo a,b"
read(*,*)a,b
write(*,*)"Debe ingresar la funci¢n f(x) antes de compilar."
write(*,*)"Aqu¡ se necesitan dos buenas aproximaciones iniciales p0 y p1."
write(*,*)"Las buenas aproximaci¢nes iniciales se har n con bisecciones."
write(*,*)"Introducir tolerancia de aproximaciones."
read(*,*)tol
x=a
fa=fun(a)
x=b
fb=fun(b)
write(*,*)"Los valores de f(a) y de f(b) son"
print*, fa,fb
p=(a+b)/2
i=0
x=p
fp=fun(x)
!
!
condd: if((fa*fb)>0)then
write(*,*)"No existe ra¡z real en dicho intervalo"
stop "Verifica intervalo"
read(*,*)
end if condd
!
!Obtenemos las buenas aproximaci¢nes inciales con bisecciones
!
cond1: if(abs(fp)<tol.or.fp==0)then
write(*,6)p,fp
6 format("El valor de p es ",f12.8," ya que f(p) vale",f12.8)
stop "acab¢"
else
do while (i<=4)
if((fp*fa)<0)then
a=a
b=p
p=(a+b)/2
x=p
fp=fun(x)
i=i+1
end if
if((fp*fb)<0)then
b=b
a=p
p=(a+b)/2
x=p
fp=fun(x)
i=i+1
end if
if(i==1)then
p0=p
end if
if(i==2)then
p1=p
end if
end do
end if cond1
i=0
fp=35
!
!
write(*,*)"La iteraci¢n del m‚todo de la posici¢n falsa es:"
write(*,*)"El n£mero de iteraci¢n, el de p y el de f(p) son:"
do while(abs(fp)>tol.or.fp/=0)
x=p0
fp0=fun(x)
x=p1
fp1=fun(x)
p=p1-(((fp1)*(p1-p0))/(fp1-fp0))
x=p
fp=fun(x)
i=i+1
write(*,5)i,p,fp
5 format(I2," ",f12.8," ",f12.8)
if(fp0*fp1<0)then
p0=p1
fp0=fp1
end if
p1=p
fp1=fp
if(abs(fp)<tol.or.fp==0.or.i>=30)then
exit
end if
end do
!
!
!
read(*,*)
end program regulafalsi
!
!
function fun (x)
real, intent(in) :: x
fun=
return
end function fun
!

