program tut7prob1
  implicit none
  real, parameter :: dt=0.001
  real, parameter :: k=1.0,m=1.0
  real, parameter :: numberOfCycles=100
  real, parameter :: omega=sqrt(k/m)
  real, parameter :: timePeriod=(2*3.14)/omega
  real, parameter :: T = numberOfCycles * timePeriod
  real, parameter :: xInit=10.0,yInit=0.0
  !real, parameter :: T=100
  integer, parameter :: N=T/dt
  !integer, parameter :: N=100
  real :: vTemp
  real, dimension(N) :: y,x,xDot,yDot

  real :: initialEnergy,finalEnergy
  integer :: i
  y(1)=yInit
  x(1)=xInit
  write(*,*) T
  initialEnergy=FindEnergy(xInit,yInit)
  open(unit=2,file="testPlot2")
  do i=2,N
     vTemp=y(i-1) + 0.5*dt*acceleration(x(i-1))
     !y(i)=y(i-1) + (-1.0*(k/m)*(x(i-1)*dt))
     !x(i)=x(i-1) + (y(i-1)*dt)
     x(i) = x(i-1) + dt*vTemp
     y(i) = vTemp + 0.5*dt*acceleration(x(i))
     if(mod(i,N/10000)==1) then
        write(2,*) x(i-1),y(i-1),(i-1)*dt,x(i-1)**2 + y(i-1)**2
     end if

  end do
  close(2)
  finalEnergy=FindEnergy(x(N),y(N))
  write(*,*) initialEnergy,FinalEnergy,((initialEnergy-FinalEnergy)/initialEnergy)*100.0, dt/timePeriod, N
  
contains
  function acceleration(xIn)
    real:: xIn,acceleration
    acceleration=-1*(k/m)*4.0*(xIn**3)
  end function acceleration

  function FindEnergy(xIn,yIn)
    real :: xIn,yIn,FindEnergy
    FindEnergy = real((yIn**2)/(2*m) + ((xIn**2)*k)/(2))
  end function FindEnergy

end program tut7prob1
