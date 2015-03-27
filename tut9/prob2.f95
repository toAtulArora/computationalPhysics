program prob2
  implicit none
  real:: x,xMinRange=-10,xMaxRange=10,eps=0.01,xRoots(100)=0,root
  integer :: k,numOfRoots=0
  integer, parameter:: numOfIt=100
  call srand(198465713)
  !write(*,*) xRoots
  do k=0,numOfIt
     x=xMinRange+(rand())*(xMaxRange-xMinRange)
     root = linePoint(x,1.0)    
     ! if(count( (root-eps) <= xRoots .and. xRoots<(root+eps)) == 0) then
     !    numOfRoots=numOfRoots+1
     !    xRoots(numOfRoots)=root
     ! end if   
     if(-10<root .and. root<10) then
        write(*,*) root
     end if
  end do
  ! write (*,*) xRoots(1:20)

contains
  function midPoint(xInit,walk)
    real,intent(in) :: xInit,walk
    real :: midPoint,x1,x2,xAvg
    integer :: k=0,j=0
    logical :: found
    found=.false.
    x1=xInit
    do k=0,10
       x2=x1 + (0.5-rand())*walk
       !write(*,*) "I started with x1,x2 = ", x1,x2
       if(f(x1)*f(x2)<0.0) then
          !write(*,*) "This is st f(x1)f(x2)<0: f(x1),f(x2)=", f(x1),f(x2)
          do j=0,100
             xAvg=(x1+x2)/2.0
             if(f(xAvg)*f(x2)<0.0) then
                x1=xAvg
             else
                x2=xAvg
             end if
             !write(*,*) "I now have new x1,x2, f(x1),f(x2):", x1,x2, f(x1),f(x2)
          end do
          !found the right x1,x2
          found=.true.
          !write(*,*) "I set found to ",found
          exit
       end if
    end do
    
       !if(f(x1)*f(x2)<0.0) then
          !found=.true.
       !else
       !end if
    !end do
    if(found) then
       midPoint=(x1+x2)/2.0
       !write(*,*) "FOUND: ",midPoint
    else
       midPoint=123456789.0
       !write(*,*) "couldn't find ",midPoint
    end if
  end function midPoint

  function linePoint(xInit,walk)
    real,intent(in) :: xInit,walk
    real :: linePoint,x1,x2,xAvg
    integer :: k=0,j=0
    logical :: found
    found=.false.
    x1=xInit
    do k=0,10
       x2=x1 + (0.5-rand())*walk
       !write(*,*) "I started with x1,x2 = ", x1,x2
       if(f(x1)*f(x2)<0.0) then
          !write(*,*) "This is st f(x1)f(x2)<0: f(x1),f(x2)=", f(x1),f(x2)
          do j=0,100
             !xAvg=(x1+x2)/2.0
             xAvg=(x1*f(x2) - x2*f(x1))/(f(x2)-f(x1))
             if(f(xAvg)*f(x2)<0.0) then
                x1=xAvg
             else
                x2=xAvg
             end if
             !write(*,*) "I now have new x1,x2, f(x1),f(x2):", x1,x2, f(x1),f(x2)
          end do
          !found the right x1,x2
          found=.true.
          !write(*,*) "I set found to ",found
          exit
       end if
    end do
    
       !if(f(x1)*f(x2)<0.0) then
          !found=.true.
       !else
       !end if
    !end do
    if(found) then
       linePoint=(x1+x2)/2.0
       !write(*,*) "FOUND: ",midPoint
    else
       linePoint=123456789.0
       !write(*,*) "couldn't find ",midPoint
    end if
  end function linePoint


  function f(x)
    real:: f,x
    f=fFlex(x,0.3)
  end function f

  function fFlex(x,lambda)
    real:: fFlex,x,lambda
    fFlex=lambda*x - sin(x)
  end function fFlex
end program prob2
