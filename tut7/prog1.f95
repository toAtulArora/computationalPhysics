program prog1
  implicit none
  real:: startX=0.0,endX=4.0*atan(1.0),pi=4.0*atan(1.0)
  integer :: numberOfIterations=100,i
  real :: stepSize,errorMax
  real:: integrationResult(2)=0
  real:: adaptiveStepSize,currentX
  real:: integrationResultAdaptive=0,smallestStep=1e-9,maxStep=1e-1
  integer :: iterationCount=0

!Initializations
  stepSize=(endX-startX)/real(numberOfIterations)


!Method 1 and 2 (midpoint and trapezoid)
  do i=0,(numberOfIterations-1)
     integrationResult(1)=integrationResult(1)+integrateFromAtoB(startX+i*stepSize,startX+(i+1)*stepSize)
     integrationResult(2)=integrationResult(2)+integrateFromAtoB(startX+i*stepSize,startX+(i+1)*stepSize,1)
  end do
  write(*,*) "The result is ", integrationResult
  !write(*,*) f(startX),f(endX),endX


!Adaptive (midpoint only)
  write(*,*) "The max error is ", error(pi/2.0,stepSize)

  errorMax=error(pi/2.0,stepSize)

  currentX=startX
  do while(currentX<endX)
     write(*,*) "Going adaptive"
     iterationCount=iterationCount+1
     adaptiveStepSize=computeStepSizeForError(errorMax,currentX)
     write(*,*) "Step Size: ", adaptiveStepSize
     integrationResultAdaptive = integrationResultAdaptive + integrateFromAtoB(currentX,currentX+adaptiveStepSize)
     currentX = currentX + adaptiveStepSize
  end do

  write(*,*) "The number of steps needed were ",iterationCount
  write(*,*) "The result is: ", integrationResultAdaptive
  !computeStepSizeForError(error(00,stepSize)

contains

  function f(x)
    real:: f,x
    f=sin(x)
  end function f

  function fDoublePrime(x)
    real::fDoublePrime,x
    fDoublePrime=-sin(x)
  end function fDoublePrime

  function integrateFromAtoB(a,b,method)
    real :: a,b,integrateFromAtoB
    integer, optional :: method

    if(present(method)) then
       integrateFromAtoB=f(a)*(b-a) + ((b-a)*(f(b)-f(a)))/2.0
    else
       integrateFromAtoB=f((a+b)/2.0)*(b-a)
    end if
  end function integrateFromAtoB

  function error(x,stepSize,method)
    real :: x,stepSize,error
    integer, optional :: method
    if(present(method)) then
       error=(fDoublePrime(x)*(stepSize**3))/12.0
    else
       error=(fDoublePrime(x)*(stepSize**3))/24.0
    end if
  end function error

  function computeStepSizeForError(err,x,method)
    real:: err,h,x,computeStepSizeForError
    integer, optional :: method
    if(present(method)) then
       !err=(f''(b-a)**3)/12.0
       if(fDoublePrime(x) .ne. 0) then
          computeStepSizeForError = (abs(((12.0*err)/fDoublePrime(x)))**(1/3.0)) + smallestStep
       else
          computeStepSizeForError=maxStep
       end if
    else
       if(fDoublePrime(x) .ne. 0) then
       !err=(f''(b-a)**3)/24.0
          computeStepSizeForError = (abs(((24.0*err)/fDoublePrime(x)))**(1/3.0)) + smallestStep
       else
          computeStepSizeForError = maxStep
       end if
    end if
  end function computeStepSizeForError
end program prog1
