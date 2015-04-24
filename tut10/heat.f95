program heat
  implicit none
  real, parameter::maxT=100,maxU=1,lambda=0.01,h=0.6,kappa=0.0001
  integer, parameter :: maxJ=maxT/h, maxL=maxU/lambda
  real, dimension(maxJ,maxL)::u
  real :: fac1,fac2
  integer :: j,l

  fac1=(1-2*kappa*h/(lambda*lambda))
  fac2=kappa*h/(lambda*lambda)
  ! write(*,*) "fac1=",fac1
  ! write(*,*) "fac2=",fac2

  !Initialize
  do l=1,maxL/2
     u(1,l)=l
  end do

  do l=maxL/2,maxL
     u(1,l)=maxL-l
  end do
  !Initilization tested to work
  !write (*,*) U(1,:)


  !Iterate
  do j=2,maxJ !j is time, j=1 is already dfined
     u(j,1)=0 !boundary condition 1
     
     do l=2,maxL-1
        u(j,l)=fac1*u(j-1,l) + fac2* ( u(j-1,l+1) + u(j-1,l-1))
     end do
     u(j,maxL)=0 !boundary condition 2
     !write(*,*) u(j,:)
  end do

  write(*,*) maxT
  ! do l=1, maxL
  !    write (*,*) u(:,l)
  ! end do
  
end program heat
