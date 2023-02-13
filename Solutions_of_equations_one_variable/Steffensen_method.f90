program steffensen
!Es aumentar la aceleraci¢n de convergencia; de lineal a cuadr tica
implicit none
real :: a,b,fa,fb,x,fun,p,fp,tol,p0,p1,fp0,fp1,gp,p2,auxg,gp0,gp1
integer :: i,j
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
read(*,*)
stop "Verifica intervalo"
read(*,*)
end if condd
!
!Iteraci¢n del punto fijo m s Aitken
cond1: if(abs(fp)<tol.or.fp==0)then
write(*,6)p,fp
6 format("El valor de p es ",f12.8," ya que f(p) vale",f12.8)
stop "acab¢"
else
j=0
write(*,*)"La iteraci¢n del m‚todo de Steffensen,"
write(*,*)"con m‚todo de Aitken cada tercera aproximaci¢n:"
write(*,*)"El n£mero de iteraci¢n, el de p y el de f(p) son:"
do while(abs(fp)>tol.or.fp/=0)
i=i+1
write(*,5)i,x,fp
5 format(I2," ",f12.8," ",f12.8)
p0=p
x=p0
gp0=auxg(x)
p1=gp0
x=p1
gp1=auxg(x)
p2=gp1
p=p0-(((p1-p0)**2)/(p2-(2*p1)+p0))
x=p
fp=fun(x)
p=x
cond2: if(abs(fp)<tol.or.fp==0.or.i>=7)then
write(*,9)i,x,fp
9 format(I2," ",f12.8," ",f12.8)
exit
end if cond2
end do
end if cond1

!
read(*,*)
end program steffensen

function fun (x)
real, intent(in) :: x
fun=x-(5**-x)
return
end function fun
!
function auxg (x)
real, intent(in) :: x
auxg=(5**-x)
return
end function auxg
!

