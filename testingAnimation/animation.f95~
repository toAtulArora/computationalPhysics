program animation
	use gnuplot_fortran
	use gnu_style
	implicit none
	
	integer, parameter :: n=100
	real, parameter :: pi = 4*atan(1.0)
	real, dimension(0:n) :: x,y
	real		:: xmin = 0, xmax= 2*pi, dx

	character(12) :: data_name
	character(12) :: frame_name
	integer :: i, frames = 250

	call system ('mkdir data')
	call system ('mkdir frames')

	dx = (xmax - xmin)/n
	x(0:n) = [(i*dx,i=0,n)]
	
	
	do i=1, frames

		write(data_name,'(a,i4.4,a)') 'data',i,'.dat'
		y=sin(0.01*i*x) / (x+1)
		call plot2d(x,y, 'data/'//data_name)

		write(frame_name,'(a,i4.4,a)') 'plot',i,'.png'

		call style_new()
		call pngcairo()
		call output ('frames/'//frame_name)
		call grid()
		call keyoff()
		call xrange(0.0,2.0*pi)
		call yrange(-1.0,1.0)
		call title('Animation')
		call datafile('data/'//data_name)
		call style_close()

		call system('gnuplot style.gnu')
	end do

	call system('ffmeg -i frames/plot%04.png animation.avi')

	call system ('rm -rf data')
	call system ('rm -rf frames')

