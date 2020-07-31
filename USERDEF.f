! --- we assume that parameter "lz1" is defined ---
      parameter (nelz=89)  !***number of elements in z direction

      parameter (pi1 = 3.1415926d0)
      parameter (vRadius = 0.25) !*** global radius of the cylinder
      parameter (vHeight = 1.00) !*** global height of the cylinder 
      Parameter (deltaT = 4.72)    !*** prescribe temperatuere at the top and bottom 
      parameter (hs = 0.20000)   ! height of the solid in units of H 

      integer*4 :: nstat,nstat1d
      real*8 :: Nu_face(2),Nu_face_avr(2)

      real*8 :: uzt_bar(lz1,nelz),t_bar(lz1,nelz),dtdz_bar(lz1,nelz)
      real*8 :: epst_bar(lz1,nelz),epsv_bar(lz1,nelz)
      real*8 :: blto_bar(lz1,nelz), blv_bar(lz1,nelz)
      real*8 :: wsqr_bar(lz1,nelz), usqrt_bar(lz1,nelz)
      real*8 :: h_flux_bar(lz1,nelz), ekin_bar(lz1,nelz)

      common /counters/ nstat,nstat1d
      common /average1/ Nu_face,Nu_face_avr
      common /average2/ uzt_bar,t_bar,dtdz_bar
      common /average3/ epst_bar,epsv_bar
      common /average4/ blto_bar, blv_bar 
      common /average5/ wsqr_bar,usqrt_bar,h_flux_bar
      common /average6/ ekin_bar






