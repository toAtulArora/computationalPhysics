program prob1
  implicit none
  real::x,y,xOld,yOld,fOld,fNew,xInit=0,yInit=0,magnitudeWalk=0.1
  integer :: attempts=0,debug=0
  integer :: k
  integer,parameter:: maxAttempts=1000,numberOfMinima=100,numberOfInitPoints=100
  real, parameter :: xInitMin=-100.0, xInitMax=100.0, yInitMin=-100.0, yInitMax=100.0
  real, dimension(100) ::xFound,yFound,fFound
  real :: xGlobal,yGlobal,fGlobal
  call srand(1248913543)

  do k=0, numberOfInitPoints
     attempts=0
     xOld=xInitMin + (xInitMax-xInitMin)*rand()
     yOld=yInitMin + (yInitMax-yInitMin)*rand()
     fOld=f(xOld,yOld)
  ! xOld=xInit
  ! yOld=yInit

     do while (attempts < maxAttempts)
        x=xOld+((0.5-rand())*magnitudeWalk)
        y=yOld+((0.5-rand())*magnitudeWalk)
        fNew=f(x,y)
        attempts=attempts+1
        if(fNew<fOld) then
           if(debug==1) then
              write(*,*) "Found after ", attempts, " attempts, a lower point"
              write(*,*) xOld,yOld,fOld
              write(*,*) x,y,fNew
           end if
           xOld=x
           yOld=y
           fOld=fNew
           attempts=0
           ! else
           ! if(debug==1) then
           !    write(*,*) "Searching.."
           ! end if
        end if
     end do
     write(*,*) "Found a minima at: ",xOld,yOld, " value: ",fOld

  end do

contains
  function f(x,y)
    real::f,x,y
    f=fFlex(x,y,0.7)
  end function f
  function fFlex(x,y,lambda)
    real::fFlex,x,y,lambda
    !fFlex=x*x + y*y + 10.0
    fFlex=(x*x*x*x) - (x*x) + y*y + lambda*(x-y)
  end function fFlex
end program prob1
