program gaussEl
	implicit none
	integer:: i,j
	integer, parameter :: N=3
	real::mat(1:N,1:N)=0,eps=1.0E-8,c(1:N)=0,x(1:N)=0,temp,knownTerms,factor
	mat=0.0
	!write(*,*) mat

	!input the matrix
		!do i=1,N
			!do j=1,N
				!mat(i,j)=i-(j*j)+5
			!end do
		!end do
	mat(1,1)=eps
	mat(1,2)=1
	mat(1,3)=(-1*eps)
	mat(2,1)=2
	mat(2,2)=1
	mat(2,3)=1
	mat(3,1)=1
	mat(3,2)=(-1*eps)
	mat(3,3)=eps

	c(1)=1
	c(2)=4
	c(3)=1	

	call printMatrix(mat)
	!Convert to row echelon form
		do i=1,(N-1)
		!i=1
			do j=(i+1),N
				factor = (mat(j,i)/mat(i,i))
				mat(j,:) = mat(j,:) - (mat(i,:)* factor)
				c(j) = c(j) - c(i)* factor
			end do
			
			call printMatrix(mat)
		end do

	!Now find the solution
	do j=N,0,-1
		!write(*,*) "evaluating.."
		knownTerms=c(j)
		!write(*,*) "known term is ", knownTerms
		do i=(j+1),N
			!write(*,*) "-",mat(j,i)*x(i)
			knownTerms= knownTerms - (mat(j,i)*x(i))
		end do
		x(j)=(knownTerms)/mat(j,j)
	end do
	write(*,*) "the solution is, ",x
!	call printMatrix(mat)

contains
	subroutine printMatrix(mat)
		real::mat(N,N)
		integer :: k
		write(*,*) "["	
		!print
			do k=1,N
				write(*,*)mat(k,:)
			end do
		write(*,*) "]"	
	end subroutine printMatrix

end program gaussEl
