program prog1
  implicit none
  real:: startX=0.0,endX=4.0*atan(1.0)
  integer :: numberOfIterations=100,i
  real :: stepSize
  real:: integrationResult(2)=0

  stepSize=(endX-startX)/real(numberOfIterations)
  do i=0,(numberOfIterations-1)
     integrationResult(1)=integrationResult(1)+integrateFromAtoB(startX+i*stepSize,startX+(i+1)*stepSize)
     integrationResult(2)=integrationResult(2)+integrateFromAtoB(startX+i*stepSize,startX+(i+1)*stepSize,1)
  end do
  write(*,*) "The result is ", integrationResult
  !write(*,*) f(startX),f(endX),endX

contains

  function f(x)
    real:: f,x
    f=sin(x)
  end function f

  function integrateFromAtoB(a,b,method)
    real :: a,b,integrateFromAtoB
    integer, optional :: method

    if(present(method)) then
       integrateFromAtoB=f(a)*(b-a) + ((b-a)*(f(b)-f(a)))/2.0
    else
       integrateFromAtoB=f((a+b)/2.0)*(b-a)
    end if
  end function integrateFromAtoB
end program prog1
