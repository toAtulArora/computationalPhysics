program tut6prob1
  implicit none
  real, parameter :: dt=0.0001
  real, parameter :: k=1.0,m=1.0
  real, parameter :: numberOfCycles=100
  real, parameter :: omega=sqrt(k/m)
  real, parameter :: timePeriod=omega/(2*3.14)
  real, parameter :: T = numberOfCycles * timePeriod
  real, parameter :: xInit=10.0,yInit=0.0
  !real, parameter :: T=100
  integer, parameter :: N=T/dt
  real, dimension(N) :: y,x,xDot,yDot

  real :: initialEnergy,finalEnergy
  integer :: i
  y(1)=yInit
  x(1)=xInit
  write(*,*) T
  initialEnergy=FindEnergy(xInit,yInit)
  open(unit=2,file="testPlot")
  do i=2,N
     y(i)=y(i-1) + (-1.0*(k/m)*(x(i-1)*dt))
     x(i)=x(i-1) + (y(i-1)*dt)
     if(mod(i,10)==1) then
        write(2,*) x(i-1),y(i-1),(i-1)*dt,x(i-1)**2 + y(i-1)**2
     end if

  end do
  close(2)
  finalEnergy=FindEnergy(x(N),y(N))
  write(*,*) initialEnergy,FinalEnergy,((initialEnergy-FinalEnergy)/initialEnergy)*100.0, dt/timePeriod, N
  
contains

  function FindEnergy(xIn,yIn)
    real :: xIn,yIn,FindEnergy
    FindEnergy = real((yIn**2)/(2*m) + ((xIn**2)*k)/(2))
  end function FindEnergy

end program tut6prob1
