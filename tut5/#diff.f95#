program diff
  implicit none
  character(10) :: fileName
  real, parameter :: xMax=5.0
  integer :: i=0,j=0
  integer,parameter :: nHvalues=4
  real :: x=0,h=0.05,euler,midpoint,fivepoint,hVals(nHvalues)

  hVals = (/ 0.1,0.05,0.01,0.005 /)
  open(1, file="output")
  do while (x<xMax)
     euler=(f(x+h) - f(x))/h
     midpoint=(f(x+h) - f(x-h))/(2*h)
     fivepoint=(8*(f(x+h)-f(x-h)) - (f(x+(2*h)) - f(x-(2*h))))/(12*h)
     !write(1,*) x,f(x), euler, df(x)-euler
     write(1,*) x,df(x)-euler,df(x)-midpoint,df(x)-fivepoint
     x=x+h
  end do  

  open(2, file="command")
  write(2,*) "set terminal jpeg"
  write(2,*) "set output 'errorForFixedHtoPointFive.jpg'"

  !write(2,*) "plot 'output' using 1:2 title 'f(x)', 'output' using 1:3 title 'euler', 'output' using 1:4 title 'error'"
  write(2,*) "plot 'output' using 1:2 title 'euler er', 'output' using 1:3 title 'midpoint er', 'output' using 1:4 title '5pt er'"
  close(2)
  close(1)
  call system("gnuplot 'command'")



  do i=1,nHvalues
     h=hVals(i)
     write(fileName,"(A,I1)") "output",i
     open(1, file=fileName)
     x=0
     do while (x<xMax)
        euler=(f(x+h) - f(x))/h
        midpoint=(f(x+h) - f(x-h))/(2*h)
        fivepoint=(8*(f(x+h)-f(x-h)) - (f(x+(2*h)) - f(x-(2*h))))/(12*h)
        !write(1,*) x,f(x), euler, df(x)-euler
        write(1,*) x,df(x)-euler,df(x)-midpoint,df(x)-fivepoint
        x=x+h
     end do     
     close(1)
  end do

  open(2,file="command")
  write(2,*) "set terminal jpeg"
  do i=1,3
     write(2,"(A,I1,A)") "set output 'errorWithMethod",i,".jpg'"
     write(2,"(A)",advance="no") "plot "
     write(2,"(A,I1,A,I1,A,f5.5,A)",advance="no") "'output",1,"' using 1:",(i+1)," title 'h=",hVals(1),"' "
     do j=2,nHvalues
        write(2,"(A,I1,A,I1,A,f5.5,A)",advance="no") ",'output",j,"' using 1:",(i+1)," title 'h=",hVals(j),"' "
     end do
     write(2,*) " "
  end do

  call system("gnuplot 'command'")

contains
  function f(x)
    real :: f,x
    f=sin(x*x)
  end function f

  function df(x)
    real :: df,x
    df=cos(x*x)*2*x
  end function df

end program diff
