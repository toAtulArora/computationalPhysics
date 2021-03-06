program gaussEl
	implicit none
	integer:: i,j,l,maxIndex
	integer, parameter :: N=8
	real::mat(1:N,1:N)=0,eps=1.0E-9,c(1:N)=0,x(1:N)=0,temp,knownTerms,factor,tempRow(1:N)
	mat=0.0
	!write(*,*) mat

	!input the matrix
		
	do i=1,N
		c(i)=0
		do j=1,N
		      	mat(i,j)=1.0/(i+j-1.0)				
			c(i)=c(i) + mat(i,j)
		end do
	end do
	! mat(1,1)=eps
	! mat(1,2)=1
	! mat(1,3)=(-1*eps)
	! mat(2,1)=2
	! mat(2,2)=1
	! mat(2,3)=1
	! mat(3,1)=1
	! mat(3,2)=(-1*eps)
	! mat(3,3)=eps

	! c(1)=1
	! c(2)=4
	! c(3)=1	

	call printMatrix(mat)
	!Convert to row echelon form
		do i=1,(N-1)
		!i=1
			!Now first find the max pivot
			maxIndex=i
			do j=(i+1),N
				if (mat(j,i) > mat(i,i) ) then	
					maxIndex=j
				end if
			end do

			!Swap if needed			
			if (maxIndex .ne. i) then
				tempRow(:) = mat(maxIndex,:)
				mat(maxIndex,:)=mat(i,:)
				mat(i,:)=tempRow(:)

				temp=c(maxIndex)
				c(maxIndex)=c(i)
				c(i)=temp
			end if

			do j=(i+1),N
				factor = (mat(j,i)/mat(i,i))
				mat(j,:) = mat(j,:) - (mat(i,:)* factor)
				c(j) = c(j) - c(i)* factor
			end do
			
			call printMatrix(mat)
			write(*,*) c
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
