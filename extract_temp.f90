!       
!       Fortran code to extract mid-plane temperateure
!        
        program extract
	implicit none
    
	integer*4, parameter :: nx = 2048, ny = 2048, nz = 256
	integer*4 :: x, y, z, ip
	real*8 :: temp
	character*5 :: nfile, nwrite
	character*80 :: fn1, fn2, fn3, fn4, fn5, fn6
	character*50 :: wf1, wf2, wf3, wf4


	DO ip = 1, 21, 10   
	 write(nfile, '(I3.3)') ip
	
	  fn4 = "temp.dat_"//nfile
	  open(14, file=TRIM(fn4), access="stream", form="unformatted")
	  do z = 1, nz/2
	   do y = 1, ny
	    do x = 1, nx
		 read(14) temp
		 if (z .eq. nz/2) then
		  wf4 = "temp_mid.dat_"//nfile
		  open(84, file = wf4, access="stream", form = 'unformatted')
		  write(84) temp
		 endif
	    enddo
	   enddo
	  enddo
	  close(84)

	  close(14)

	ENDDO

	
	stop
	end program 
