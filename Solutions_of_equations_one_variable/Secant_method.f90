program metsecante
implicit none
real :: a,b,fa,fb,x,fun,p,fp,tol,p0,p1,fp0,fp1
integer, parameter :: dp = selected_real_kind(16, 307)
!c1=30764755123, c2=36456545223, c3=5691846094
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
read(*,*)
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
do while (i<=100)
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
if(i==2)then
p0=p
end if
if(i==3)then
p1=p
end if
end do
end if cond1
i=0
fp=35
!
write(*,*)"La iteraci¢n del m‚todo de la secante es:"
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
5 format(I2," ",f14.10," ",f14.10)
p0=p1
p1=p
if(abs(fp)<tol.or.fp==0.or.i>=30)then
exit
end if
end do
!
!
!
read(*,*)
end program metsecante
!
!151326*(79973+((179323)*(2.71828**(-10*x))))-203302*(27997*(2.71828**(-20*x))+179323*(2.71828**(-10*x)))
function fun (x)
real, intent(in) :: x
fun=
return
end function fun
!
!75+0.22222222*(3*(x**2)-6*x)-0.0311111119*(4*(x**3)-18*(x**2)+18*x)&
!&-0.0064444395*(5*(x**4)-44*(x**3)+117*(x**2)-90*x)&
!&+0.002638887*(6*(x**5)-80*(x**4)+376*(x**3)-720*(x**2)-450*x)&
!&-0.0009131194439*(7*(x**6)-144*(x**5)+1125*(x**4)-3698*(x**3)+6429*(x**2)-3600*x)&
!&+0.000130526794*(8*(x**7)-224*(x**6)+2484*(x**5)-13840*(x**4)+40324*(x**3)&
!-56880*(x**2)+28800*x)-0.000020223633*(9*(x**8)-360*(x**7)+5810*(x**6)-48900*(x**5)&
!&+230325*(x**4)-600052*(x**3)+782640*(x**2)-374400*x)-80.667




