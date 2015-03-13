program prob2
  integer :: numIterations=1000000,iteration=0,insideCount=0,seed
  real :: x,y,z,volumeOctet,pi(10000)

  ! do seed=2,1000
  !    call srand(seed)
  call srand(86456)
     do while (iteration<numIterations)
        iteration=iteration+1
        x=rand()
        y=rand()
        z=rand()
        if( (x*x) + (y*y) + (z*z) < 1.0) then
           insideCount=insideCount+1
        end if
     end do

     volumeOctet=real(insideCount)/numIterations
     write(*,*) "Pi is ",volumeOctet*6.0
  ! end do                        
end program prob2
