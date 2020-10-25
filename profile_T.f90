!	To caclualate mean temperature of binary file format 
!       written from interpolation subroutine
	program extract_temp
	implicit none
  
	integer*4, parameter :: nx = 2048, ny = 2048, nz = 256  ! number of grids in x, y, z
	integer*4 :: x, y, z, ip, cnt
	real*8 :: temp
	real*8 :: t_tavg(1:nz), t_tavg1(1:nz)
	real*8 :: t_savg(1:nz), t_savg1(1:nz)
	character*5 :: nfile, nwrite
	character*80 :: fn1, fn2, fn3, fn4, fn5, fn6
	character*50 :: gl_file




	cnt = 0 ! counting total number of frames
	DO ip = 10
	 t_tavg(:) = 0.0
	 t_savg(:) = 0.0

	  print*, ip
	  write(nfile, "(I3.3)")ip
	  fn1 = "temp.dat_"//nfile
	  open(14, file=TRIM(fn1), access="stream", form="unformatted")
	  do z = 1, nz
	   do y = 1, ny
	 	do x = 1, nx
		 read(14) temp
		 t_tavg(z) = t_tavg(z) + temp
		 t_savg(z) = t_savg(z) + temp**2
		enddo
	   enddo
	  enddo
	  close(14)

	 t_tavg(:) = t_tavg(:)/(nx*ny)
	 t_savg(:) = t_savg(:)/(nx*ny)

! Take root-mean-square
	 t_savg(:) = sqrt(t_savg(:))
	 cnt = cnt+1
	

	fn2 = "T_z.dat_"//nfile
	fn3 = "T_rms.dat_"//nfile

	open(82, file = trim(fn2), form = 'formatted')
	open(83, file = trim(fn3), form = 'formatted')
	do z = 1, nz
	 write(82, *) real((z-1))/(nz-1), t_tavg(z)
	 write(83, *) real((z-1))/(nz-1), t_savg(z)
	enddo
	close(82)
	close(83)
	
	ENDDO


	
	stop
	end program 
