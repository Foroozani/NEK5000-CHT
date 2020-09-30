   program TecplotSnapshots
   implicit none
! fortran code reads data from interpolated field from NEK5000 code 
! and apply sliding-time-average to plot streamlines, which can be 
! visualized with Tecplot360. Note it is written for a cylinder of Gamma = 0.5
! execute with
!  ifort -O2 -i4 -r8 *****.f90 -o *****.exe 
! --- control parameters ---
   integer*4, parameter :: nskip = 1
   integer*4, parameter :: nskip_xy = 1 ! for Tecplot output
   integer*4, parameter :: tecplot_mode = 1 ! 0: append, 1: create new

! --- data arrays ---
   real*8, pointer :: var1(:,:), var2(:,:), var3(:,:), var4(:,:)
   real*8, pointer :: vx(:,:,:), vy(:,:,:), vz(:,:,:), tt(:,:,:)
   real*8, pointer :: fx(:,:,:), fy(:,:,:), fz(:,:,:), ft(:,:,:)
   real*8, pointer :: uh(:,:,:)

   character*80 :: fname, fname1
   character*80 :: title,title1,title2,title3 
   integer*4 :: i,j,k
   integer*4 :: ii,im,ntime

! --- grid dimensions from interpolated field ---
   integer*4, save :: nx = 256
   integer*4, save :: ny = 256
   integer*4, save :: nz = 512

! --- cylinder size ---
   real*8, save :: Radius = 0.25
   real*8, save :: Height = 1.00

! --- grid coordinates ---
   real*8 :: xx,yy,zz,rr
   real*8 :: ftime

! --- memory allocation ---
   call mem_alloc(nx,ny,nz)

   ntime = 0

   !im = 10 ! number of snapshots

! --- start loop over snapshots from which snapshots
   do ii = 50,50

      write(*,*) 'ii=',ii

      write(fname,'("velx.dat_",I3.3)') ii
      call read_field_bin(nx,ny,nz,TRIM(fname),fx) ! => vx-velocity

      write(fname,'("vely.dat_",I3.3)') ii
      call read_field_bin(nx,ny,nz,TRIM(fname),fy) ! => vy-velocity

      write(fname,'("velz.dat_",I3.3)') ii
      call read_field_bin(nx,ny,nz,TRIM(fname),fz) ! => vz-velocity

      write(fname,'("temp.dat_",I3.3)') ii
      call read_field_bin(nx,ny,nz,TRIM(fname),ft) ! => temperature

      ftime = float(ntime)

      ! --- compute time-averaging ---
      do k = 1,nz
      do j = 1,ny
      do i = 1,nx

      ! velocity components averaged in time
         vx(i,j,k) = (fx(i,j,k) + ftime*vx(i,j,k))/(ftime + 1.0)
         vy(i,j,k) = (fy(i,j,k) + ftime*vy(i,j,k))/(ftime + 1.0)
         vz(i,j,k) = (fz(i,j,k) + ftime*vz(i,j,k))/(ftime + 1.0)

      ! temperature field averaged in time
         tt(i,j,k) = (ft(i,j,k) + ftime*tt(i,j,k))/(ftime + 1.0)

           
      enddo
      enddo
      enddo

      ntime = ntime + 1
        
   enddo ! - ii cycle
          


! --- tecplot output ---
     call write_tecplot(nx,ny,nz,'3d_tecplot_ins.dat',vx,vy,vz,tt,'Flow 3D')
! --- freeing up memory ---
   call mem_free(nx,ny,nz)
     
   stop '**OK, processing is done !**'
    
   contains


! ************************************
!     memory allocation/deallocation
! ************************************
   subroutine mem_alloc(nx,ny,nz)
      implicit none

      ! dummy arguments
      integer*4 :: nx,ny,nz

      ! 3D arrays for time-averaged flow fields
      allocate(vx(1:nx,1:ny,1:nz), vy(1:nx,1:ny,1:nz), vz(1:nx,1:ny,1:nz), tt(1:nx,1:ny,1:nz))
      allocate(uh(1:nx,1:ny,1:nz))
       
      ! 3D arrays for instantaneous flow fields
      allocate(fx(1:nx,1:ny,1:nz), fy(1:nx,1:ny,1:nz), fz(1:nx,1:ny,1:nz), ft(1:nx,1:ny,1:nz))

      return
   end subroutine mem_alloc

!------------------------------------
   subroutine mem_free(nx,ny,nz)
      implicit none

      ! dummy arguments
      integer*4 :: nx,ny,nz

      ! deallocate 3D data
      deallocate(vx, vy, vz, tt, uh)
      deallocate(fx, fy, fz, ft)

      return
   end subroutine mem_free


! ************************************
!      read complete 3D field
! ************************************
   subroutine read_field_bin(nx,ny,nz,fname,f)
      implicit none

      ! dummy arguments
      integer*4 :: nx,ny,nz
      character(*) :: fname
      real*8, pointer :: f(:,:,:)

      ! local variables
      integer*4 :: i,j,k

      write(*,'(A,1X,A)') '  - read_field_bin:',fname

      open(unit=80,file=fname,status='old',access='stream',form='unformatted')
      
      ! read flow field
      do k = 1,nz
      do j = 1,ny
      do i = 1,nx

         read(80) f(i,j,k)

      enddo
      enddo
      enddo

      close(80)

      return
   end subroutine read_field_bin

! ************************************
!      save snapshot to teclpot file, 3D 
! ************************************
   subroutine write_tecplot(nx,ny,nz,fname,v1,v2,v3,tt,title)
      implicit none

      ! dummy arguments
      integer*4 :: nx,ny,nz ! XYZ-dimensions
      character(*) :: fname,title
      real*8, pointer :: v1(:,:,:), v2(:,:,:), v3(:,:,:), tt(:,:,:)

      ! local variables
      integer*4 :: i,j,k
      real*8 :: xx,yy,zz,rr

503   format(es13.6,1X,es13.6,(3(1X,es13.6)))
504   format(es13.6,1X,es13.6,(4(1X,es13.6)))
506   format(es13.6,1X,es13.6,1X,es13.6,(6(1X,es13.6)))

      write(*,'(A,1X,A)') '  - write_tecplot file in:',fname

      if (tecplot_mode.eq.0) then
         open(unit=90,file=fname,position='append')
      else
         open(unit=90,file=fname,status='unknown')
      endif

      ! --- create tecplot header ---
      write(90,*) 'title = "',title,'"'
      write(90,*) 'variables = "x", "y", "z", "r", "vx", "vy", "vz", "urms","tt"'
      write(90,*) 'zone i=', nx, ', j=', ny, ', k=', nz, ', f=point'

      ! --- write flow field as XY-planes ---
      do k = 1,nz
         do j = 1,ny
         do i = 1,nx
         ! construct grid coords
            xx = 2.0*Radius*(i-1)/float(nx-1) - Radius
            yy = 2.0*Radius*(j-1)/float(ny-1) - Radius
            zz = 1.0*Height*(k-1)/float(nz-1)

         ! compute actual radius
            rr = sqrt(xx**2 + yy**2)

        write(90,506) xx,yy,zz,rr,v1(i,j,k),v2(i,j,k),v3(i,j,k),sqrt(v1(i,j,k)**2+v2(i,j,k)**2+v3(i,j,k)**2),tt(i,j,k)
         enddo
         ! add blank line
            write(90,*)
         enddo
      enddo

      close(90)

      return
   end subroutine write_tecplot


!_______________________________________________________________________


   end program TecplotSnapshots
