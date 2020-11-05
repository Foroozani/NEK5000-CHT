        program 
        implicit none
 
        integer, parameter :: nx = 512, ny = 512
        integer :: x, y, sn
        real*8 :: T(1:nx, 1:ny), T_mean(1:nx,1:ny)
        character*80 :: fn1, fn2, fn3, fn4
        character*8 :: nfile, nfile1


        sn = 15 ! snapshot number for slicing window
        wl = 5 ! length of window

        if (mod(wl,2) .eq. 0) then
         imin = sn-wl/2   
         imax = sn+wl/2-1
        else
         imin = sn-(wl-1)/2
         imax = sn+(wl-1)/2
        endif


        T_mean(:,:) = 0.0
        cnt1 = 0

         do sn1 = imin, imax
          write(nfile, '(I3.3)') sn1
          print*, nfile


          fn1 = 'temp_z.dat_'//nfile
          open(11, file = fn1, form = 'unformatted', access = 'stream')
          do y = 1, ny
           do x = 1, nx
                read(11) T(x,y)
           enddo
          enddo

          T_mean(:,:) = T_mean(:,:) + T(:,:)
          close(11)  
   
          cnt1 = cnt1+1
         enddo
         print*, cnt1
         T_mean(:,:) = T_mean(:,:)/cnt1

        write(nfile1, '(I3.3)') sn
        fn2 = 'mean_temp_z.dat_'//nfile1
        open(12, file = fn2, form = 'unformatted', access = 'stream')
        do y = 1, ny
         do x = 1, nx
          write(12) T_mean(x,y)
         enddo
        enddo

        

        print*, ' Done'

        stop
        end program
