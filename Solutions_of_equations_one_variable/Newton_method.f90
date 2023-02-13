program metnewton
implicit none
real :: a,b,fa,fb,x,fun,p,fp,tol,fpri,devf,aux,p0
integer :: i,opc
write(*,*)"Debe ingresar la funci¢n f(x), y la"
write(*,*)"derivada de dicha funci¢n antes de correr el programa."
write(*,*)"Introducir tolerancia de aproximaciones."
read(*,*)tol
write(*,*)"La aproximaci¢n inicial es con bisecciones dado un intervalo,"
write(*,*)"o con un punto inicial. Si es la primera opci¢n"
write(*,*)"Presione 1, si es con la segunda, presionar 2. "
read(*,*)opc
!
!
!
if(opc==1)then
write(*,*)"Definir intervalo a,b"
read(*,*)a,b
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
condd: if((fa*fb)>0)then
write(*,*)"No existe ra¡z real en dicho intervalo"
stop "Verifica intervalo"
read(*,*)
end if condd
!Obtenemos una buena aproximaci¢n incial con bisecciones
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
end do
end if cond1
end if
!
!
!
if(opc==2)then
write(*,*)"Introducir punto de aproximaci¢n inicial"
read(*,*)p0
end if
i=0
!
write(*,*)"La iteraci¢n del m‚todo de Newton es:"
!Aqu¡ se implementa el m‚todo de Newton
write(*,*)"El n£mero de iteraci¢n, el de p y el de f(p) son:"
do while(i<=30)
i=i+1
p=p0-(fun(p0)/fpri(p0))
fp=fun(p)
write(*,5)i,p,fp
5 format(I2," ",f12.8," ",f12.8)
if(abs(fp)<tol.or.fp==0)then
exit
end if
p0=p
end do
!
read(*,*)
end program metnewton

!
function fun (x)
real, intent(in) :: x
fun=151326*(79973+((179323)*(2.71828**(-10*x))))-203302*(27997*(2.71828**(-20*x))+179323*(2.71828**(-10*x)))
return
end function fun
!
function fpri (x)
real, intent(in) :: x
fpri=
return
end function fpri

