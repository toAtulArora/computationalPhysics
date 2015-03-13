!gauss quadrature
program prob1
  implicit none
  real:: startX=0.0,endX=1.0,incX=0.001,x,summedIntegrate
  write(*,*) "Just defined some functions"
  x=startX
  summedIntegrate=0.0
  do while (x<=endX)
     summedIntegrate=summedIntegrate+integrate(x,x+incX)
     x=x+incX
  end do
  write(*,*) "The integral is ", summedIntegrate
contains
  function alpha(x1,x2)
    real:: alpha,x1,x2
    alpha=2.0/(x2-x1)
  end function alpha
  function beta(x1,x2)
    real:: beta,x1,x2
    beta=-(x1+x2)/(x2-x1)
  end function beta
  function f(x)
    real::f,x
    f=x
    ! f=sin(x)/x
  end function f
  function g(y,alpha,beta)
    real::g,y,alpha,beta
    g=f((y-beta)/alpha)/alpha
  end function g
  function integrate(x1,x2)
    real::integrate,x1,x2,al,be,a2=1/sqrt(3.0)
    al=alpha(x1,x2)
    be=beta(x1,x2)
    integrate=g(-a2,al,be) + g(a2,al,be)
  end function integrate
end program prob1
